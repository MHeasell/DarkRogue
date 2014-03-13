(ns darkrogue.util-test
  (:require [clojure.test :refer :all]
            [darkrogue.util :refer :all]))

(deftest test-remove-vals
  (testing "that remove-vals works"
           (is (= {:a 1 :b 2}
                  (remove-vals
                    #(> % 2)
                    {:a 1 :b 2 :c 3 :d 4})))))

(run-tests)
