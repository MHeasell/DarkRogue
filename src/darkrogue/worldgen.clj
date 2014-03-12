(ns darkrogue.worldgen)

(use 'darkrogue.graph)
(use 'darkrogue.coord)
(use 'darkrogue.grid)

(def sample-graph (make-graph (coords-in-rect 0 0 3 3)))

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