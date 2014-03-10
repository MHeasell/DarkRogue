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

(deftest test-grid
  (testing "checks that grid get/set works"
           (is (= 0 (get-cell (make-grid 2 2 0) 0 0)))))

(run-tests)
