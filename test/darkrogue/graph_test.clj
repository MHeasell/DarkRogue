(ns darkrogue.graph-test
  (:require [clojure.test :refer :all]
            [darkrogue.graph :refer :all]))

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

(deftest test-node-pairs [graph]
  (testing "checks node pair generation"
           (is (= (set  [[:a :b] [:b :a]])
                  (set (node-pairs (graph :a :b)))))))

(deftest test-unconnected-pairs [graph]
  (testing "tests unconnected-pairs"
           (is (= (set [[:a :b] [:b :a]])
                  (set (-> (graph :a :b :c)
                         (add-edge :a :c)
                         (add-edge :b :c)
                         (unconnected-pairs)))))))
