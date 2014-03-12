(ns darkrogue.universe
  (:require [clojure.test :refer :all]
            [darkrogue.coord :refer :all] 
            [darkrogue.universe :refer :all]
            [darkrogue.grid :refer :all]))

(def example-terrain (make-grid 3 3 :empty))

(deftest test-move
  (testing "tests that movement moves the player"
           (is (= (make-coord 3 5)
                  (-> (make-universe example-terrain)
                    (spawn-player (make-coord 1 2))
                    (move-player (make-coord 2 3))
                    (get-in [:player :position]))))))

(run-tests)
