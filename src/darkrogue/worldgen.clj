(ns darkrogue.worldgen)

(use 'darkrogue.graph)
(use 'darkrogue.coord)
(use 'darkrogue.grid)

(def WORLD_WIDTH 3)

(def WORLD_HEIGHT 3)

(def WORLD_TILE_WIDTH 50)

(def WORLD_TILE_HEIGHT 30)

(def sample-graph (make-graph (coords-in-rect 0 0 3 3)))

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

(defn connect-nodes-from [graph node]
  (let [node-set (set (nodes graph))
        bros (filter #(contains? node-set %1) (neighbours node))
        unconnected-bros (remove #(connected-to-any? graph %1) bros)]
    (if (seq unconnected-bros)
      (let [candidate (rand-nth unconnected-bros)]
        (connect-nodes-from
          (add-edge graph node candidate)
          candidate))
      graph)))

(defn connect-initial-chain [graph]
  (connect-nodes-from graph (rand-nth (nodes graph))))

(defn connect-remaining-nodes [graph]
  (let [unconnected (remove (partial connected-to-any? graph) (nodes graph))]
    (if (seq unconnected)
      (let [node-set (set (nodes graph))
            candidate (rand-nth unconnected)
            bros (filter #(contains? node-set %) (neighbours candidate))
            connected-bros (filter #(connected-to-any? graph %) bros)]
        (if (seq connected-bros)
          (connect-remaining-nodes (add-edge graph candidate (rand-nth connected-bros)))
          (connect-remaining-nodes graph))
        )
      graph)))

(defn generate-world-graph [width height]
  (-> (make-graph (coords-in-rect 0 0 width height))
    (connect-initial-chain)
    (connect-remaining-nodes)
    (add-random-connections (rand-int width))))

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
