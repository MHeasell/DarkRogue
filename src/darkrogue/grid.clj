(ns darkrogue.grid)

(use 'darkrogue.coord)
(use 'darkrogue.util)

; Grid stuff

(defrecord Grid [width height elements defaultval])

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
    (get-cell grid (make-coord x y))))

(defn put-cell [grid coord value]
  (Grid.
    (:width grid)
    (:height grid)
    (assoc (:elements grid) coord value)
    (:defaultval grid)))

(defn fill-rect [grid x y width height val]
 (reduce #(put-cell %1 %2 val) grid (coords-in-rect x y width height)))

(defn fill-rect-extents
  ([grid coords extent-x extent-y val]
    (fill-rect-extents grid (:x coords) (:y coords) extent-x extent-y val))
  ([grid center-x center-y extent-x extent-y val]
    (fill-rect grid
               (- center-x extent-x)
               (- center-y extent-y)
               (+ (* extent-x 2) 1)
               (+ (* extent-y 2) 1)
               val)))

(defn print-grid-row [grid row]
  (println (map #(get-cell grid %1 row) (range 0 (:width grid)))))

(defn print-grid [grid]
  (dorun (map #(print-grid-row grid %1) (range 0 (:height grid)))))

(defn map-grid [f grid]
  (Grid.
    (:width grid)
    (:height grid)
    (fmap f (:elements grid))
    (f (:defaultval grid))))
