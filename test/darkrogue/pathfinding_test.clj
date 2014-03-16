(ns darkrogue.pathfinding-test
  (:use [clojure.test]
        [darkrogue.pathfinding]
        [clojure.tools.trace]))


(deftest test-astar
  (testing "that astar works..."
           (is (= [0 1 2 3]
                  (astar
                    #(- 3 %)
                    #(hash-map (inc %) 1 (dec %) 1)
                    0
                    #(= % 3))))))

(run-tests)
