(ns darkrogue.universe
  (:require [darkrogue.coord :as c]))


(def PLAYER_HP 100)

(defrecord Player [position health])

(defrecord Universe [player terrain])

(defn make-universe [terrain]
  (Universe. nil terrain))

(defn spawn-player [universe position]
  (assoc universe :player (Player. position PLAYER_HP)))

(defn move-player [universe offset]
  (update-in universe [:player :position] (partial c/add-coord offset)))
