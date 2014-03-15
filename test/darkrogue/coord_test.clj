(ns darkrogue.coord-test
  (:require [clojure.test :refer :all]
            [darkrogue.coord :refer :all]))

(deftest test-neighbours
  (testing "neighbours"
           (is (= (list (make-coord 1 3)
                        (make-coord 3 3)
                        (make-coord 2 2)
                        (make-coord 2 4))
                  (neighbours (make-coord 2 3))))))

(deftest test-add-coord
  (testing "test coord addition"
           (is (= (make-coord 3 5)
                  (add-coord (make-coord 1 2) (make-coord 2 3))))))

(deftest test-coords-in-rect
  (testing "checks coords in rectangle"
           (is (= (list (make-coord 3 3))
                  (coords-in-rect 3 3 1 1)))
           (is (= (list (make-coord 4 4)
                        (make-coord 5 4)
                        (make-coord 4 5)
                        (make-coord 5 5))
                  (coords-in-rect 4 4 2 2)))))


(deftest test-in-rect
  (testing "checks that in-rect works"
           (is (true?
                 (in-rect? (make-coord 2 3) 1 1 4 4)))
           (is (false?
                 (in-rect? (make-coord 3 3) 0 0 3 3)))))


(deftest test-distance
  (testing "checks distance func"
           (is (= 5.0
                  (distance (make-coord 1 1) (make-coord 4 5))))))

(run-tests)
