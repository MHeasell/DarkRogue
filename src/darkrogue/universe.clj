(ns darkrogue.universe
  (:require [darkrogue.coord :as c])
  (:require [darkrogue.grid :as g]))


(def PLAYER_HP 100)

(defrecord Player [position health])

(defrecord Universe [player terrain])

(defn make-universe [terrain]
  (Universe. nil terrain))

(defn spawn-player [universe position]
  (assoc universe :player (Player. position PLAYER_HP)))

(defn is-obstacle? [terrain coord]
  (= :wall (g/get-cell terrain coord)))

(defn universe-width [universe]
  (get-in universe [:terrain :width]))

(defn universe-height [universe]
  (get-in universe [:terrain :height]))

(defn point-occupied? [universe coord]
  (is-obstacle? (:terrain universe) coord))

(defn put-tile [universe coord val]
  (assoc universe :terrain (g/put-cell (:terrain universe) coord val)))

(defn move-player [universe offset]
  (let
    [newpos (c/add-coord (get-in universe [:player :position]) offset)]
    (if (not (point-occupied? universe newpos))
      (assoc-in universe [:player :position] newpos)
      universe)))
