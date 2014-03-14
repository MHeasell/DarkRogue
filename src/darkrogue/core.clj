(ns darkrogue.core)

(require '[lanterna.screen :as s])
(use 'darkrogue.util)
(use 'darkrogue.graph)
(use 'darkrogue.coord)
(use 'darkrogue.grid)
(use 'darkrogue.worldgen)
(use 'darkrogue.universe)

; level drawing stuff

(defn get-glyph [sym]
  (cond
    (= :floor sym) \.
    (= :wall sym) \#
    (= :corridor sym) \~
    (= :empty sym) \space
    :else \space))

(defn get-display-item [grid coord]
  (let [area-coords (coords-in-rect (- (:x coord) 1) (- (:y coord) 1) 3 3)]
    (if (every? #(= :wall (get-cell grid %)) area-coords)
      :empty
      (get-cell grid coord))))

(defn get-glyph-in-universe [universe coord]
  (get-glyph (get-display-item (:terrain universe) coord)))

(defn draw-level [screen universe coords]
  (let [screen-size (s/get-size screen)
        screen-width (second screen-size)
        screen-height (first screen-size)]
    (dorun
      (map #(s/put-string screen (:x %) (:y %) (str (get-glyph-in-universe universe (add-coord % coords))))
          (for [y (range screen-width)
                x (range screen-height)]
            (make-coord x y))))))


(def PLAYER_GLYPH \@)
(def ENEMY_GLYPH \G)
(def BIG_BAD_GLYPH \@)

(defn draw-player [screen player offset]
  (let [computed-coord (add-coord (:position player) offset)]
    (s/put-string screen
                  (:x computed-coord)
                  (:y computed-coord)
                  (str PLAYER_GLYPH)
                  {:fg :white})))

(defn draw-guard [screen enemy offset]
  (let [computed-coord (add-coord (:position enemy) offset)]
    (s/put-string screen
                  (:x computed-coord)
                  (:y computed-coord)
                  (str ENEMY_GLYPH)
                  {:fg :yellow})))

(defn draw-big-bad [screen enemy offset]
  (let [computed-coord (add-coord (:position enemy) offset)]
    (s/put-string screen
                  (:x computed-coord)
                  (:y computed-coord)
                  (str BIG_BAD_GLYPH)
                  {:fg :red})))

(defn draw-enemy [screen enemy offset]
  (cond (= :guard (:type enemy)) (draw-guard screen enemy offset)
        (= :big-bad (:type enemy)) (draw-big-bad screen enemy offset)))

(defn draw-enemies [screen universe offset]
  (dorun (map #(draw-enemy screen % offset) (vals (:enemies universe)))))

(defn draw-universe [screen universe]
  (let [camera-offset (make-coord 0 0)]
    (draw-level screen universe camera-offset)
    (draw-enemies screen universe camera-offset)
    (draw-player screen (:player universe) camera-offset)))

; player commands

(def unit-left (make-coord -1 0))
(def unit-right (make-coord 1 0))
(def unit-up (make-coord 0 -1))
(def unit-down (make-coord 0 1))

(defn context-action [universe offset]
  (let [new-coord (add-coord offset
                             (get-in universe [:player :position]))
        enemy (get-enemy-at universe new-coord)]
    (if enemy
      (hit-enemy-offset universe offset)
      (move-player universe offset))))

(defn go-left [universe]
  (context-action universe unit-left))

(defn go-right [universe]
  (context-action universe unit-right))

(defn go-up [universe]
  (context-action universe unit-up))

(defn go-down [universe]
  (context-action universe unit-down))

; main game initialization

(def input-command-mapping
  {:left go-left
   :right go-right
   :up go-up
   :down go-down
   \w go-up
   \a go-left
   \s go-down
   \d go-right
   \x (fn [u] nil)})

(defn apply-input [universe input]
  (let [f (get input-command-mapping input)]
    (if f
      (f universe)
      universe)))

(defn tick-universe [universe]
  (remove-dead-enemies universe))

(defn game-win [screen universe]
  (s/put-string screen 20 10 (apply str (repeat 25 \space)))
  (s/put-string screen 20 11 (apply str (repeat 25 \space)))
  (s/put-string screen 20 12 (apply str (repeat 25 \space)))
  (s/put-string screen 21 11 "Assassination complete!")
  (s/redraw screen)
  (loop [key (s/get-key-blocking screen)]
    (if (= \x key)
      nil
      (recur (s/get-key-blocking screen)))))

(defn game-loop [screen universe]
    (draw-universe screen universe)
    (s/redraw screen)
    (let [new-universe (apply-input universe (s/get-key-blocking screen))]
      (when new-universe
      (if (is-game-won? new-universe)
        (game-win screen universe)
        (recur screen (tick-universe new-universe))))))

(defn random-point-in-universe [universe]
  (make-coord (rand-int (universe-width universe))
              (rand-int (universe-height universe))))

(defn find-empty-spot [universe]
  (let [candidate (random-point-in-universe universe)]
    (if (point-occupied? universe candidate)
      (find-empty-spot universe)
      candidate)))

(defn spawn-random [universe spawn-func]
  (spawn-func universe (find-empty-spot universe)))

(defn init-universe []
  (-> (generate-world)
    (make-universe)
    (spawn-random spawn-player)
    (spawn-random spawn-enemy)
    (spawn-random spawn-enemy)
    (spawn-random spawn-enemy)
    (spawn-random spawn-big-bad)
    ))

(defn start-game [screen]
  (game-loop screen (init-universe)))

(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (s/put-string screen 0 0 "DarkRogue" {:styles #{:bold}})
                 (s/put-string screen 0 1 "A RogueLike game by Michael Heasell")
                 (s/put-string screen 0 2 "Press any key to start...")
                 (s/redraw screen)
                 (s/get-key-blocking screen)
                 (start-game screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text") :text
                      :else :auto)]
    (main screen-type)))
