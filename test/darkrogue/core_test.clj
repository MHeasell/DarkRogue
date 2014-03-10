(ns darkrogue.core-test
  (:require [clojure.test :refer :all]
            [darkrogue.core :refer :all]))

(deftest test-make-graph
  (testing "check that making a graph works"
           (is (= {:a #{} :b #{}}
                  (graph :a :b)))))
(deftest test-make-graph
  (testing "checks that making a graph works"
           (is (= {:a #{} :b #{} :c #{}}
                  (make-graph '(:a :b :c))))))

(deftest test-connected
  (testing "checks that connected predicate works"
           (is (false?
                 (connected? (graph :a :b) :a :b)))
           (is (true?
                 (connected? (add-edge (graph :a :b) :a :b) :a :b)))))

(deftest test-connected-to-any
  (testing "checks that connected-to-any predicate works"
           (is (false?
                 (connected-to-any? (graph :a) :a)))
           (is (true?
                 (-> (graph :a :b)
                   (add-edge :a :b)
                   (connected-to-any? :a))))))

(deftest test-grid-get-put
  (testing "checks that grid get/put works"
           (is (= 0 (get-cell (make-grid 2 2 0) 0 0)))
           (is (= 3 (-> (make-grid 2 2 0)
                      (put-cell (make-coord 1 1) 3)
                      (get-cell (make-coord 1 1)))))))

(deftest test-coords-in-rect
  (testing "checks coords in rectangle"
           (is (= (list (make-coord 3 3))
                  (coords-in-rect 3 3 1 1)))
           (is (= (list (make-coord 4 4)
                        (make-coord 5 4)
                        (make-coord 4 5)
                        (make-coord 5 5))
                  (coords-in-rect 4 4 2 2)))))

(run-tests)
