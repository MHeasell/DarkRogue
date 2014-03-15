(ns darkrogue.graph)

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

(defn connected-pairs [graph]
  (filter #(apply (partial connected? graph) %) (node-pairs graph)))

(defn add-random-edge [graph]
  (apply (partial add-edge graph) (rand-nth (unconnected-pairs graph))))

(defn add-random-connections [graph num]
  (if (= num 0)
    graph
    (if (seq (unconnected-pairs graph))
      (add-random-connections (add-random-edge graph) (- num 1))
      graph)))
