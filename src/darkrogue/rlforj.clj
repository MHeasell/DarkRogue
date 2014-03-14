(ns darkrogue.rlforj
  (:require [darkrogue.coord :as c]))

(import rlforj.los.ILosBoard)
(import rlforj.los.IConeFovAlgorithm)
(import rlforj.los.ShadowCasting)
(import rlforj.los.ConePrecisePremisive)

(def ^:dynamic visible-set)

(defn getfov [blocking-pred start-coord startangle finishangle]
  (binding [visible-set #{}]
    (let [board (proxy [ILosBoard] []
                  (visit [x y] (set! visible-set (conj visible-set (c/make-coord x y))))
                  (isObstacle [x y] (boolean (blocking-pred (c/make-coord x y))))
                  (contains [x y] (c/in-rect? (c/make-coord x y) 0 0 500 500)))]
      (.visitConeFieldOfView (ShadowCasting.)
        board
        (:x start-coord)
        (:y start-coord)
        10
        startangle
        finishangle)
      visible-set)))
