(ns darkrogue.universe
  (:require [darkrogue.util :as u])
  (:require [darkrogue.coord :as c])
  (:require [darkrogue.grid :as g]))


(def PLAYER_HP 100)

(def ENEMY_HP 100)

(defrecord Player [position health])

(defrecord Enemy [position health])

; terrain is a grid of cells
; enemies is a map of coord -> enemy data
(defrecord Universe [player terrain enemies])

(defn make-universe [terrain]
  (Universe. nil terrain {}))

(defn make-enemy [position health]
  (Enemy. position health))

(defn add-enemy [universe enemy]
  (assoc-in universe [:enemies (:position enemy)] enemy))

(defn spawn-player [universe position]
  (assoc universe :player (Player. position PLAYER_HP)))

(defn spawn-enemy [universe position]
  (assoc-in universe [:enemies position] (Enemy. position ENEMY_HP)))

(defn is-obstacle? [terrain coord]
  (= :wall (g/get-cell terrain coord)))

(defn universe-width [universe]
  (get-in universe [:terrain :width]))

(defn universe-height [universe]
  (get-in universe [:terrain :height]))

(defn point-occupied? [universe coord]
  (or (is-obstacle? (:terrain universe) coord)
      (contains? (:enemies universe) coord)))

(defn put-tile [universe coord val]
  (assoc universe :terrain (g/put-cell (:terrain universe) coord val)))

(defn move-player [universe offset]
  (let
    [newpos (c/add-coord (get-in universe [:player :position]) offset)]
    (if (not (point-occupied? universe newpos))
      (assoc-in universe [:player :position] newpos)
      universe)))

(defn get-enemy-at [universe coord]
  (get-in universe [:enemies coord]))

(defn damage-enemy [enemy points]
  (assoc enemy :health (- (:health enemy) points)))

(defn hit-enemy [universe coord]
  (let [enemy (get-enemy-at universe coord)]
    (when enemy
      (assoc-in universe [:enemies coord] (damage-enemy enemy 100)))))

(defn hit-enemy-offset [universe offset]
  (hit-enemy universe (c/add-coord (get-in universe [:player :position]) offset)))

(defn is-dead? [enemy]
  (<= (:health enemy) 0))

(defn remove-dead-enemies [universe]
  (update-in universe [:enemies] (partial u/remove-vals is-dead?)))
