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

(deftest test-move-enemies
  (testing "tests that enemies block movement"
           (is (= (make-coord 1 2)
                  (-> (make-grid 3 3 :floor)
                    (make-universe)
                    (spawn-player (make-coord 1 2))
                    (spawn-enemy (make-coord 1 1))
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
                   (point-occupied? (make-coord 2 2)))))
           (is (true?
                 (->
                   (make-grid 3 3 :empty)
                   (make-universe)
                   (spawn-player (make-coord 1 1))
                   (point-occupied? (make-coord 1 1)))))))

(deftest test-get-enemy
  (testing "tests enemy retrieval"
           (is (= (make-enemy (make-coord 1 1) 100 :up)
                 (-> (make-grid 3 3 :wall)
                   (make-universe)
                   (spawn-enemy (make-coord 1 1) :up)
                   (get-enemy-at (make-coord 1 1)))))))

(deftest test-hit-enemy
  (testing "tests that enemies get damaged properly"
           (is (= 0
                  (-> (make-grid 3 3 :wall)
                   (make-universe)
                   (spawn-enemy (make-coord 1 1))
                   (hit-enemy (make-coord 1 1))
                   (get-enemy-at (make-coord 1 1))
                   (:health))))))

(deftest test-remove-dead-enemies
  (testing "tests that dead enemies are removed"
           (is (= {(make-coord 1 1) (make-enemy (make-coord 1 1) 10)}
                  (-> (make-grid 3 3 :wall)
                   (make-universe)
                   (add-enemy (make-enemy (make-coord 1 1) 0))
                   (add-enemy (make-enemy (make-coord 1 1) -1))
                   (add-enemy (make-enemy (make-coord 1 1) 10))
                   (remove-dead-enemies)
                   (:enemies))))))

(deftest test-is-game-won?
  (testing "tests that the game is won when big bad is dead"
           (is (true?
                 (-> (make-grid 3 3 :floor)
                   (make-universe)
                   (add-enemy (make-big-bad (make-coord 1 1) 0))
                   (is-game-won?))))
           (is (false?
                 (-> (make-grid 3 3 :floor)
                   (make-universe)
                   (add-enemy (make-enemy (make-coord 1 1) 0))
                   (is-game-won?))))
           (is false?
               (-> (make-grid 3 3 :floor)
                   (make-universe)
                   (add-enemy (make-big-bad (make-coord 1 1) 10))
                   (is-game-won?)))))

(deftest test-blocks-vision?
  (testing "tests that vision is blocked properly"
           (is (true?
                 (-> (make-grid 3 3 :floor)
                   (make-universe)
                   (put-tile (make-coord 1 1) :wall)
                   (blocks-vision? (make-coord 1 1)))))
           (is (false?
                 (-> (make-grid 3 3 :floor)
                   (make-universe)
                   (blocks-vision? (make-coord 1 1)))))))

(deftest test-can-see-player
  (testing "tests that can-see-player updates correctly"
           (let [uni (-> (make-grid 3 3 :floor)
                                (make-universe)
                                (spawn-player (make-coord 1 1))
                                (spawn-enemy (make-coord 1 2) :up)
                                (update-enemy-vision))]
             (is (true?
                   (can-see-player? uni (get-enemy-at uni (make-coord 1 2))))))))

(run-tests)
