(ns darkrogue.core-test
  (:require [clojure.test :refer :all]
            [darkrogue.coord :refer :all]
            [darkrogue.grid :refer :all]
            [darkrogue.universe :refer :all]
            [darkrogue.core :refer :all]))

(deftest test-apply-input
  (testing "tests that input gives the desired action"
           (is (= (make-coord 1 0)
                  (->
                    (make-grid 3 3 :floor)
                    (make-universe)
                    (spawn-player (make-coord 1 1))
                    (apply-input :up)
                    (get-in [:player :position]))))))

(run-tests)
