(ns darkrogue.coord)

(defrecord Coordinate [x y])

(defn make-coord [x y]
  (Coordinate. x y))

(def unit-left (make-coord -1 0))
(def unit-right (make-coord 1 0))
(def unit-up (make-coord 0 -1))
(def unit-down (make-coord 0 1))
(def zero (make-coord 0 0))

(def direction-coord-map
  {:left unit-left
   :right unit-right
   :up unit-up
   :down unit-down})

(defn add-coord [a b]
  (make-coord
    (+ (:x a) (:x b))
    (+ (:y a) (:y b))))

(defn sub-coord [a b]
  (make-coord
    (- (:x a) (:x b))
    (- (:y a) (:y b))))

(defn in-rect? [coord x y width height]
  (and (>= (:x coord) x)
       (>= (:y coord) y)
       (< (:x coord) (+ x width))
       (< (:y coord) (+ y height))))

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

(defn length [c]
  (Math/sqrt (+
              (* (:x c) (:x c))
              (* (:y c) (:y c)))))

(defn distance [a b]
  (length (sub-coord b a)))

(defn apply-movement [coord direction]
  (add-coord coord (get direction-coord-map direction)))
