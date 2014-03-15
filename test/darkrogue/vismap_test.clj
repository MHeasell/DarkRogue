(ns darkrogue.vismap-test
  (:require [clojure.test :refer :all]
            [darkrogue.vismap :refer :all]))

(deftest test-mark-visible
  (testing "tests marking items as visible"
           (is (false?
                 (visible-from? empty-vismap 1 2))) 
           (is (true?
                 (-> empty-vismap
                   (mark-visible 1 2)
                   (visible-from? 1 2))))))

(deftest test-mark-targets-visible
  (testing "tests marking multiple targets"
           (let [m (mark-targets-visible empty-vismap 1 [2 3])]
             (is (true? (visible-from? m 2 1)))
             (is (true? (visible-from? m 3 1))))))

(run-tests)
