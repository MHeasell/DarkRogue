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

(defn draw-level [screen level coords]
  (let [screen-size (s/get-size screen)
        screen-width (second screen-size)
        screen-height (first screen-size)
        ]
    (dorun
      (map #(s/put-string screen (:x %) (:y %) (str (get-glyph (get-display-item level (add-coord % coords)))))
          (for [y (range screen-width)
                x (range screen-height)]
            (make-coord x y))))))

(def PLAYER_GLYPH \@)

(defn draw-player [screen player offset]
  (let [computed-coord (add-coord (:position player) offset)]
    (s/put-string screen (:x computed-coord) (:y computed-coord) (str PLAYER_GLYPH))))

(defn draw-universe [screen universe]
  (let [camera-offset (make-coord 0 0)]
    (draw-level screen (:terrain universe) camera-offset)
    (draw-player screen (:player universe) camera-offset)))

; player commands

(defn go-left [universe]
  (move-player universe (make-coord -1 0)))

(defn go-right [universe]
  (move-player universe (make-coord 1 0)))

(defn go-up [universe]
  (move-player universe (make-coord 0 -1)))

(defn go-down [universe]
  (move-player universe (make-coord 0 1)))


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
   \x #(java.lang.System/exit 0)})

(defn apply-input [universe input]
  (let [f (get input-command-mapping input)]
    (when f
      (f universe))))

(defn game-loop [screen universe]
    (draw-universe screen universe)
    (s/redraw screen)
    (recur screen
           (apply-input universe (s/get-key-blocking screen))))

(defn main-loop [screen]
  (draw-level screen (generate-world) (make-coord 0 0))
  (s/redraw screen)
  (if (= (s/get-key-blocking screen) \d)
    nil
    (recur screen)))

(defn random-point-in-universe [universe]
  (make-coord (rand-int (universe-width universe))
              (rand-int (universe-height universe))))

(defn find-empty-spot [universe]
  (let [candidate (random-point-in-universe universe)]
    (if (point-occupied? universe candidate)
      (find-empty-spot universe)
      candidate)))

(defn start-game [screen]
  (let [world (generate-world)
        universe (make-universe world)
        ready-universe (spawn-player universe (find-empty-spot universe))]
    (game-loop screen ready-universe)))

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
