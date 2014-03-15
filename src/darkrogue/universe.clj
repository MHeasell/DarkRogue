(ns darkrogue.universe
  (:require [darkrogue.util :as u])
  (:require [darkrogue.coord :as c])
  (:require [darkrogue.grid :as g])
  (:require [darkrogue.rlforj :as los])
  (:require [darkrogue.vismap :as vis])
  (:require [clojure.set :as s]))


(def PLAYER_HP 100)

(def ENEMY_HP 100)

(def BIG_BAD_HP 100)

(defrecord Player [position health])

(defrecord Enemy [position health type facing state])
(def directions #{:up :down :left :right})
(defn random-direction []
  (rand-nth (seq directions)))

; terrain is a grid of cells
; enemies is a map of coord -> enemy data
; vismap is a map of coord -> set of occupied coords that can see this
(defrecord Universe [player terrain enemies vismap messages])

(defn add-message [universe msg]
  (assoc universe :messages (conj (:messages universe) msg)))

(defn clear-messages [universe]
  (assoc universe :messages []))

(defn make-universe [terrain]
  (Universe. nil terrain {} {} []))

(defn make-enemy
  ([position health direction]
    (Enemy. position health :guard direction :passive))
  ([position health]
    (make-enemy position health :up)))

(defn make-big-bad
  ([position health direction]
    (Enemy. position health :big-bad direction :passive))
  ([position health]
    (make-big-bad position health :up)))

(defn add-enemy [universe enemy]
  (assoc-in universe [:enemies (:position enemy)] enemy))

(defn spawn-player [universe position]
  (assoc universe :player (Player. position PLAYER_HP)))

(defn spawn-enemy
  ([universe position direction]
    (assoc-in universe [:enemies position]
              (make-enemy position ENEMY_HP direction)))
  ([universe position] (spawn-enemy universe position (random-direction))))

(defn spawn-big-bad [universe position]
  (assoc-in universe [:enemies position]
            (make-big-bad position BIG_BAD_HP (random-direction))))

(defn is-obstacle? [terrain coord]
  (= :wall (g/get-cell terrain coord)))

(defn universe-width [universe]
  (get-in universe [:terrain :width]))

(defn universe-height [universe]
  (get-in universe [:terrain :height]))

(defn point-occupied? [universe coord]
  (or (is-obstacle? (:terrain universe) coord)
      (contains? (:enemies universe) coord)
      (and (contains? universe :player)
           (= (get-in universe [:player :position]) coord))))

(defn blocks-vision? [universe coord] (is-obstacle? (:terrain universe) coord))

(defn put-tile [universe coord val]
  (assoc universe :terrain (g/put-cell (:terrain universe) coord val)))

(defn move-player [universe offset]
  (let
    [newpos (c/add-coord (get-in universe [:player :position]) offset)]
    (if (not (point-occupied? universe newpos))
      (assoc-in universe [:player :position] newpos)
      universe)))

(defn move-enemy [universe enemy offset direction]
  (let [newpos (c/add-coord (:position enemy) offset)]
    (if (not (point-occupied? universe newpos))
      (update-in universe [:enemies] #(-> %
                                        (dissoc (:position enemy))
                                        (assoc newpos (assoc enemy :position newpos :facing direction))))
      universe)))

(defn get-enemy-at [universe coord]
  (get-in universe [:enemies coord]))

(defn alerted? [enemy]
  (= :alerted (:state enemy)))

(defn damage-enemy [enemy points]
  (assoc enemy :health (- (:health enemy) points)))

(defn perform-stealth-kill [universe coord]
  (-> universe
    (update-in [:enemies coord] #(damage-enemy % 100))
    (add-message "You stealthily extinguish your enemy's life.")))

(defn perform-attack [universe coord]
  (-> universe
    (update-in [:enemies coord] #(damage-enemy % 40))
    (add-message "You hit your enemy.")))

(defn hit-enemy [universe coord]
  (let [enemy (get-enemy-at universe coord)]
    (when enemy
      (if (alerted? enemy)
        (perform-attack universe coord)
        (perform-stealth-kill universe coord)))))

(defn hit-enemy-offset [universe offset]
  (hit-enemy universe (c/add-coord (get-in universe [:player :position]) offset)))

(defn is-dead? [enemy]
  (<= (:health enemy) 0))

(defn is-big-bad? [enemy]
  (= (:type enemy) :big-bad))

(defn is-game-won? [universe]
  (boolean (some (every-pred is-big-bad? is-dead?) (vals (:enemies universe)))))

(defn remove-dead-enemies [universe]
  (update-in universe [:enemies] (partial u/remove-vals is-dead?)))


; visibility functions

(def angles {:up [225 315]
             :down [45 135]
             :left [135 225]
             :right [315 45]})

(defn calculate-fov [universe coord direction]
  (let [start-angle (first (get angles direction))
        end-angle (second (get angles direction))]
    (los/getfov (partial blocks-vision? universe) coord start-angle end-angle)))

(defn calculate-enemy-visible-set [universe enemy]
  (calculate-fov universe (:position enemy) (:facing enemy)))

(defn calculate-new-vismap [universe]
  (reduce #(vis/mark-targets-visible %1 (:position %2) (calculate-enemy-visible-set universe %2))
          {}
          (vals (:enemies universe))))

(defn update-enemy-vision [universe]
  (assoc universe :vismap (calculate-new-vismap universe)))

(defn visible-by-any? [universe coord]
  (vis/visible-from-any? (:vismap universe) coord))

(defn visible-by? [universe coord fromcoord]
  "returns true if coord is visible from fromcoord"
  (vis/visible-from? (:vismap universe) coord fromcoord))

(defn player-visible-by? [universe fromcoord]
  (visible-by? universe (get-in universe [:player :position]) fromcoord))

(defn can-see-player? [universe enemy]
  (player-visible-by? universe (:position enemy)))

; AI logic

(def ai-actions
  {:wait (fn [universe enemy] universe)
   :up (fn [universe enemy] (move-enemy universe enemy c/unit-up :up))
   :down (fn [universe enemy] (move-enemy universe enemy c/unit-down :down))
   :left (fn [universe enemy] (move-enemy universe enemy c/unit-left :left))
   :right (fn [universe enemy] (move-enemy universe enemy c/unit-right :right))})

(defn ai-action-func [enemy action]
  #((get ai-actions action) % enemy))

(defn become-alerted [enemy]
  (assoc enemy :state :alerted))

(defn become-passive [enemy]
  (assoc enemy :state :passive))

(defn observe [universe enemy]
  "produces a new enemy state based on the observed universe"
  (if (can-see-player? universe enemy)
    (become-alerted enemy)
    enemy))

(def direction-coord-map {:left c/unit-left
                          :right c/unit-right
                          :up c/unit-up
                          :down c/unit-down
                          :wait c/zero})

(defn apply-movement [coord direction]
  (c/add-coord coord (get direction-coord-map direction)))

(defn score-move [start dest move]
  (c/distance (apply-movement start move) dest))

(defn get-best-direction [start goal]
  (min-key #(score-move start goal %)
           :up
           :down
           :left
           :right))

(defn decide-action-alerted [universe enemy]
  "decides AI action when in alerted state"
  (let [target (get-in universe [:player :position])
        action (get-best-direction (:position enemy) target)]
    action))

(defn decide-action [universe enemy]
  "returns an action for the enemy to take"
  (if (alerted? enemy)
    (decide-action-alerted universe enemy)
    :wait))

(defn process-ai-move [universe enemy]
  (-> universe
    (update-in [:enemies (:position enemy)] #(observe universe %))
    ((ai-action-func enemy (decide-action universe enemy)))))
