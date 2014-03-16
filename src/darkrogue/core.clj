(ns darkrogue.core
  (:require [lanterna.screen :as s])
  (:use darkrogue.util)
  (:use darkrogue.graph)
  (:use darkrogue.coord)
  (:use darkrogue.grid)
  (:use darkrogue.worldgen)
  (:use darkrogue.universe))


(def SMOKE_BOMB_RANGE 10)

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

(defn draw-level-tile [screen universe level-coord screen-coord]
  (if (smoked? universe level-coord)
    (s/put-string screen (:x screen-coord) (:y screen-coord) "S" {:fg :green})
    (let [glyph-str (str (get-glyph-in-universe universe level-coord))
          attrs (if (visible-by-any? universe level-coord) {:bg :white :fg :black} {})]
      (s/put-string screen (:x screen-coord) (:y screen-coord) glyph-str attrs))))

(defn draw-level [screen universe coords]
  (let [screen-size (s/get-size screen)
        screen-width (second screen-size)
        screen-height (first screen-size)]
    (dorun
      (map #(draw-level-tile screen universe (add-coord % coords) %)
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

(defn draw-messages [screen messages]
  (dorun (map-indexed #(s/put-string screen 0 %1 %2) messages)))

(defn draw-universe [screen universe]
  (let [camera-offset (make-coord 0 0)]
    (s/move-cursor screen 0 0)
    (draw-level screen universe camera-offset)
    (draw-enemies screen universe camera-offset)
    (draw-player screen (:player universe) camera-offset)
    (draw-messages screen (:messages universe))))

; player commands

(defn ask-for-coord [initial screen message]
  (s/put-string screen 0 0 message)
  (s/move-cursor screen (:x initial) (:y initial))
  (s/redraw screen)
  (let [input (case (s/get-key-blocking screen)
                (\w :up) :up
                (\a :left) :left
                (\d :right) :right
                (\s :down) :down
                :escape :exit
                :enter :confirm
                nil)]
    (case input
      :exit nil
      :confirm initial
      (:left :right :up :down) (recur (apply-movement initial input)
                                      screen
                                      message)
      (recur initial screen message))))

(defn ask-for-coord-limited [initial screen message valid-coords]
  (s/put-string screen 0 0 message)
  (s/move-cursor screen (:x initial) (:y initial))
  (s/redraw screen)
  (let [input (case (s/get-key-blocking screen)
                (\w :up) :up
                (\a :left) :left
                (\d :right) :right
                (\s :down) :down
                :escape :exit
                :enter :confirm
                nil)]
    (case input
      :exit (do
              (s/put-string screen 0 0 (apply str (repeat 50 \space)))
              (s/put-string screen 0 0 "Never mind.")
              (s/move-cursor screen 0 0)
              (s/redraw screen))
      :confirm initial
      (:left :right :up :down) (let [newcoord (apply-movement initial input)]
                                 (recur (if (contains? valid-coords newcoord) newcoord initial)
                                        screen
                                        message
                                        valid-coords))
      (recur initial screen message valid-coords))))

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

(defn apply-input [screen universe input]
  (if (= \b input)
    (let [player-pos (get-in universe [:player :position])
          valid-coords (calculate-wide-fov universe player-pos SMOKE_BOMB_RANGE)
          coord (ask-for-coord-limited player-pos screen "Throw smoke bomb where?" valid-coords)]
      (if coord
        (throw-smoke-bomb universe coord)
          (recur screen universe (s/get-key-blocking screen))))
    (let [f (get input-command-mapping input)]
      (if f
        (f universe)
        (recur screen universe (s/get-key-blocking screen))))))

(defn tick-universe [universe]
  (-> universe
    (remove-dead-enemies)
    (update-enemy-vision)))

(defn apply-ai-moves [universe]
  (reduce #(tick-universe (process-ai-move %1 %2))
          universe
          (vals (:enemies universe))))

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

(defn game-lose [screen universe]
  (s/put-string screen 20 10 (apply str (repeat 25 \space)))
  (s/put-string screen 20 11 (apply str (repeat 25 \space)))
  (s/put-string screen 20 12 (apply str (repeat 25 \space)))
  (s/put-string screen 21 11 "Killed! You have failed.")
  (s/redraw screen)
  (loop [key (s/get-key-blocking screen)]
    (if (= \x key)
      nil
      (recur (s/get-key-blocking screen)))))

(defn game-loop [screen universe]
    (draw-universe screen universe)
    (s/redraw screen)
    (let [new-universe (apply-input screen (clear-messages universe) (s/get-key-blocking screen))]
      (when new-universe
        (cond
          (is-game-won? new-universe) (game-win screen new-universe)
          (is-game-lost? new-universe) (game-lose screen new-universe)
          :else (recur screen (update-enemy-vision (update-smoke (apply-ai-moves (tick-universe new-universe)))))))))

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
    (spawn-random spawn-enemy)
    (spawn-random spawn-enemy)
    (spawn-random spawn-enemy)
    (spawn-random spawn-big-bad)
    (tick-universe)
    (add-message "You awaken...")))

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
