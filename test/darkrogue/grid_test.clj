(ns darkrogue.grid-test
  (:require [clojure.test :refer :all]
            [darkrogue.coord :refer :all]
            [darkrogue.grid :refer :all]))

(deftest test-grid-get-put
  (testing "checks that grid get/put works"
           (is (= 0 (get-cell (make-grid 2 2 0) 0 0)))
           (is (= 3 (-> (make-grid 2 2 0)
                      (put-cell (make-coord 1 1) 3)
                      (get-cell (make-coord 1 1)))))))
