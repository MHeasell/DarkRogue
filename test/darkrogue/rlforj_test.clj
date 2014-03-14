(ns darkrogue.rlforj-test
  (:require [clojure.test :refer :all]
            [darkrogue.coord :refer :all]
            [darkrogue.rlforj :refer :all]))

(deftest test-cone-fov
  (testing "test that interop with the library works okay"
           (let [start (make-coord 1 1)
                 space (set (coords-in-rect 0 0 3 3))]
             (is (= (conj (set (coords-in-rect 0 0 3 1)) start)
                    (getfov #(contains? space %) start 225 315))))))

(run-tests)
