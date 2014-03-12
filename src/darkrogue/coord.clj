(ns darkrogue.coord)

(defrecord Coordinate [x y])

(defn make-coord [x y]
  (Coordinate. x y))

(defn add-coord [a b]
  (make-coord
    (+ (:x a) (:x b))
    (+ (:y a) (:y b))))

(defn sub-coord [a b]
  (make-coord
    (- (:x a) (:x b))
    (- (:y a) (:y b))))

(defn coords-in-rect [x y width height]
  (for [y (range y (+ height y))
        x (range x (+ width x))]
    (make-coord x y)))

(defn neighbours [coord]
  (list (make-coord (- (:x coord) 1) (:y coord))
        (make-coord (+ (:x coord) 1) (:y coord))
        (make-coord (:x coord) (- (:y coord) 1))
        (make-coord (:x coord) (+ (:y coord) 1))))

(defn neighbour-eight [coord]
  (for [dy [-1 0 1]
        dx [-1 0 1]
        :when (not (= 0 dy dx))]
    (make-coord
      (+ (:x coord) dx)
      (+ (:y coord) dy))))

(defn get-line-coords [x1 y1 x2 y2]
  (let [minx (min x1 x2)
        miny (min y1 y2)
        maxx (max x1 x2)
        maxy (max y1 y2)
        horiz-coords (map #(make-coord % y1) (range minx (+ maxx 1)))
        vert-coords (map #(make-coord x2 %) (range miny (+ maxy 1)))
        all-coords (concat horiz-coords vert-coords)]
    all-coords))