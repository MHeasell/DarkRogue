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

(defn move-player [universe offset]
  (update-in universe [:player :position] (partial c/add-coord offset)))

(defn is-obstacle? [terrain coord]
  (= :wall (g/get-cell terrain coord)))

(defn universe-width [universe]
  (get-in universe [:terrain :width]))

(defn universe-height [universe]
  (get-in universe [:terrain :height]))

(defn point-occupied? [universe coord]
  (is-obstacle? (:terrain universe) coord))
