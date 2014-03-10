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

(deftest test-grid-get-put
  (testing "checks that grid get/put works"
           (is (= 0 (get-cell (make-grid 2 2 0) 0 0)))
           (is (= 3 (-> (make-grid 2 2 0)
                      (put-cell (make-coord 1 1) 3)
                      (get-cell (make-coord 1 1)))))))

(run-tests)
