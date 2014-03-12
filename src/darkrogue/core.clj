(ns darkrogue.core)

(require '[lanterna.screen :as s])
(use 'darkrogue.util)
(use 'darkrogue.graph)
(use 'darkrogue.coord)
(use 'darkrogue.grid)
(use 'darkrogue.worldgen)

(defn draw-line
  ([grid val x1 y1 x2 y2]
    (let [coords (get-line-coords x1 y1 x2 y2)]
      (reduce
        #(if (= (get-cell grid %2) :wall)
           (put-cell %1 %2 val)
           %1)
        grid coords)))
  ([grid val start end]
    (draw-line grid val (:x start) (:y start) (:x end) (:y end))))

; misc utilities

(defn get-glyph [sym]
  (cond
    (= :floor sym) \.
    (= :wall sym) \#
    (= :corridor sym) \~
    (= :empty sym) \space
    :else \space))

(def WORLD_WIDTH 3)

(def WORLD_HEIGHT 3)

(def WORLD_TILE_WIDTH 50)

(def WORLD_TILE_HEIGHT 30)

(defn compute-room-center [coord cell-width cell-height]
  (make-coord
    (+ (* (:x coord) cell-width) (quot cell-width 2))
    (+ (* (:y coord) cell-height) (quot cell-height 2))))

(defn generate-world []
  (let [world-graph (generate-world-graph WORLD_WIDTH WORLD_HEIGHT)
        blank-grid (make-grid WORLD_TILE_WIDTH WORLD_TILE_HEIGHT :wall)
        cell-width (quot WORLD_TILE_WIDTH WORLD_WIDTH)
        cell-height (quot WORLD_TILE_HEIGHT WORLD_HEIGHT)
        filled-rooms (reduce
                       #(fill-rect-extents %1
                                           (compute-room-center %2 cell-width cell-height)
                                           (rand-int (quot cell-width 2))
                                           (rand-int (quot cell-height 2))
                                           :floor)
                       blank-grid
                       (coords-in-rect 0 0 WORLD_WIDTH WORLD_HEIGHT))
        connected-rooms (reduce
                          #(apply (partial draw-line %1 :corridor) %2)
                          filled-rooms
                          (map #(vector (compute-room-center (first %) cell-width cell-height) (compute-room-center (second %) cell-width cell-height))
                               (connected-pairs world-graph)))]
    connected-rooms))

; main screen displaying stuff

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

(defn main-loop [screen]
  (draw-level screen (generate-world) (make-coord 0 0))
  (s/redraw screen)
  (if (= (s/get-key-blocking screen) \d)
    nil
    (recur screen)))

(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (s/put-string screen 0 0 "DarkRogue" {:styles #{:bold}})
                 (s/put-string screen 0 1 "A RogueLike game by Michael Heasell")
                 (s/put-string screen 0 2 "Press any key to generate level, D to exit.")
                 (s/redraw screen)
                 (s/get-key-blocking screen)
                 (main-loop screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text") :text
                      :else :auto)]
    (main screen-type)))
