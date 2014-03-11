(ns darkrogue.core)

(require '[lanterna.screen :as s])


; Coordinate stuff

(defrecord Coordinate [x y])

(defn make-coord [x y]
  (Coordinate. x y))

(defn coords-in-rect [x y width height]
  (for [y (range y (+ height y))
        x (range x (+ width x))]
    (make-coord x y)))


; Grid stuff

(defrecord Grid [width height elements defaultval])

(defn make-grid [width height value]
  "Creates a grid of the given width and height, filled with the given value."
  (Grid. width height {} value))

(defn get-cell
  ([grid coord]
    (let [val (get (:elements grid) coord)]
      (if val
        val
        (:defaultval grid))))
  ([grid x y]
    (get-cell grid (Coordinate. x y))))

(defn put-cell [grid coord value]
  (Grid.
    (:width grid)
    (:height grid)
    (assoc (:elements grid) coord value)
    (:defaultval grid)))

(defn fill-rect [grid x y width height val]
 (reduce #(put-cell %1 %2 val) grid (coords-in-rect x y width height)))

(defn print-grid-row [grid row]
  (println (map #(get-cell grid %1 row) (range 0 (:width grid)))))

(defn print-grid [grid]
  (dorun (map #(print-grid-row grid %1) (range 0 (:height grid)))))


; misc utilities

(defn neighbours-in-grid [grid coord]
  (filter #(and (>= (:x coord) 0)
                (>= (:y coord) 0)
                (< (:x coord) (:width grid))
                (< (:y coord) (:height grid)))))

(defn neighbours [coord]
  (list (make-coord (- (:x coord) 1) (:y coord))
        (make-coord (+ (:x coord) 1) (:y coord))
        (make-coord (:x coord) (- (:y coord) 1))
        (make-coord (:x coord) (+ (:y coord) 1))))


; graph routines

(def empty-graph {})

(defn add-node [graph val]
  (assoc graph val #{}))

(defn make-graph [coll]
  (reduce add-node empty-graph coll))

(defn graph [& items]
  (make-graph items))

(defn add-directed-edge [graph n1 n2]
  (let [n1-neighbours (get graph n1)]
    (assoc graph n1 (conj n1-neighbours n2))))

(defn add-edge [graph n1 n2]
  (-> graph
    (add-directed-edge n1 n2)
    (add-directed-edge n2 n1)))

(defn connected? [graph n1 n2]
  (contains? (get graph n1) n2))

(defn connected-to-any? [graph node]
  (boolean (seq (get graph node))))

(defn nodes [graph]
  (keys graph))

(defn node-pairs [graph]
  (for [a (nodes graph)
        b (nodes graph)
        :when (not= a b)]
    [a b]))

(defn all-connected? [graph]
  (every? #(boolean (seq %)) (map second graph)))

(defn unconnected-pairs [graph]
  (remove #(apply (partial connected? graph) %) (node-pairs graph)))

(defn add-random-edge [graph]
  (apply (partial add-edge graph) (rand-nth (unconnected-pairs graph))))

(defn add-random-connections [graph num]
  (if (= num 0)
    graph
    (if (seq (unconnected-pairs graph))
      (add-random-connections (add-random-edge graph) (- num 1))
      graph)))

; world generation

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

(defn generate-world-graph
  (-> (make-graph (coords-in-rect 0 0 3 3))
    (connect-initial-chain)
    (connect-remaining-nodes)
    (add-random-connections (rand-int 3))))

; main screen displaying stuff

(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (s/put-string screen 0 0 "DarkRogue" {:styles #{:bold}})
                 (s/put-string screen 0 1 "A RogueLike game by Michael Heasell")
                 (s/put-string screen 0 2 "Press any key to exit...")
                 (s/redraw screen)
                 (s/get-key-blocking screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text") :text
                      :else :auto)]
    (main screen-type)))
