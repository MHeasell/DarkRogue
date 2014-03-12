(ns darkrogue.universe-test
  (:require [clojure.test :refer :all]
            [darkrogue.coord :refer :all] 
            [darkrogue.universe :refer :all]
            [darkrogue.grid :refer :all]))

(def example-terrain (make-grid 3 3 :floor))

(deftest test-move
  (testing "tests that movement moves the player"
           (is (= (make-coord 3 5)
                  (-> (make-universe example-terrain)
                    (spawn-player (make-coord 1 2))
                    (move-player (make-coord 2 3))
                    (get-in [:player :position]))))))

(deftest test-move-obstacles
  (testing "tests that obstacles block movement"
           (is (= (make-coord 1 2)
                  (-> (make-grid 3 3 :floor)
                    (make-universe)
                    (spawn-player (make-coord 1 2))
                    (put-tile (make-coord 1 1) :wall)
                    (move-player (make-coord 0 -1))
                    (get-in [:player :position]))))))

(deftest test-width
  (testing "tests universe width function"
           (is (= 4 (-> (make-grid 4 5 :empty)
                      (make-universe)
                      (universe-width))))))

(deftest test-height
  (testing "tests universe width function"
           (is (= 2 (-> (make-grid 2 6 :empty)
                      (make-universe)
                      (universe-width))))))

(deftest test-point-occupied?
  (testing "tests that point-occupied works"
           (is (true?
                 (->
                   (make-grid 3 3 :wall)
                   (make-universe)
                   (point-occupied? (make-coord 2 2)))))
           (is (false?
                 (->
                   (make-grid 3 3 :empty)
                   (make-universe)
                   (point-occupied? (make-coord 2 2)))))))

(run-tests)
