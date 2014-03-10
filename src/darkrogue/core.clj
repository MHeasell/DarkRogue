(ns darkrogue.core)

(require '[lanterna.screen :as s])


; world  data

(defrecord Coordinate [x y])

(defrecord Grid [width height elements defaultval])

(defn make-coord [x y]
  (Coordinate. x y))

(defn make-grid [width height value]
  "Creates a grid of the given width and height, filled with the given value."
  (Grid. width height {} value))

(defn get-cell
  ([grid coord]
    (let [val (get (:elements grid) coord)]
      (if val
        val
        (:defaultval grid))))
  ([grid x y]
    (get-cell grid (Coordinate. x y))))

(defn put-cell [grid coord value]
  (Grid.
    (:width grid)
    (:height grid)
    (assoc (:elements grid) coord value)
    (:defaultval grid)))

(defn print-grid-row [grid row]
  (println (map #(get-cell grid %1 row) (range 0 (:width grid)))))

(defn print-grid [grid]
  (dorun (map #(print-grid-row grid %1) (range 0 (:height grid)))))

; graph routines

(defn graph [& items]
  (make-graph items))

(defn make-graph [coll]
  (reduce add-node empty-graph coll))

(def empty-graph {})

(defn add-directed-edge [graph n1 n2]
  (let [n1-neighbours (n1 graph)]
    (assoc graph n1 (conj n1-neighbours n2))))

(defn add-edge [graph n1 n2]
  (-> graph
    (add-directed-edge n1 n2)
    (add-directed-edge n2 n1)))

(defn add-node [graph val]
  (assoc graph val #{}))

(defn connected? [graph n1 n2]
  (contains? (n1 graph) n2))


(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (s/put-string screen 0 0 "DarkRogue" {:styles #{:bold}})
                 (s/put-string screen 0 1 "A RogueLike game by Michael Heasell")
                 (s/put-string screen 0 2 "Press any key to exit...")
                 (s/redraw screen)
                 (s/get-key-blocking screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text") :text
                      :else :auto)]
    (main screen-type)))
