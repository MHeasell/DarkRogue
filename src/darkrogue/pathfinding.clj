(ns darkrogue.pathfinding
  (:use [clojure.data.priority-map]))

(defn get-path-helper [start goal visited succ h]
    (if (= start goal)
      nil
      (let [successors (succ start)
            unvisited (remove #(contains? visited (second %)) successors)
            next (apply min-key #(h (second %)) unvisited)]
        (cons (first next) (get-path-helper (second next)
                                            goal
                                            (conj visited start)
                                            succ
                                            h)))))

(defn get-path [start goal succ h]
  "finds a path using greedy best first search"
  (get-path-helper start goal #{} succ h))
  


(defn ^:dynamic astar [h succ start goal?]
  "h - heuristic estimating remaining distance from given node
   succ(node) - yields successors, a map of node -> cost
   start - the starting node
   goal? - predicate, returns true if current node is the goal"
  (loop [open (priority-map start [0 nil])
         closed {}]
    (let [[node [cost parent]] (first open)]
      (cond
        (nil? node) "path not found"
        (goal? node) (->> (conj
                            (iterate #(second (get closed %)) parent)
                            node)
                       (take-while #(not (nil? %)))
                       reverse)
        :else (recur
                (merge-with #(if (< (first %1) (first %2)) %1 %2)
                            (dissoc open node)
                            (into {} (for [[n ncost] (succ node)
                                          :when (not (contains? closed n))]
                                      [n [(+ ncost cost (h n)) node]])))
                (assoc closed node [cost parent]))))))
