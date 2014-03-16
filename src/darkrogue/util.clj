(ns darkrogue.util)

(defn abs [x]
  (if (neg? x) (- x) x))

(defn fmap [f m]
  "applies the function f to all the values of the map m"
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-vals [pred map]
  "removes keys from the map whose values satisfy pred"
  (select-keys map (remove #(pred (get map %)) (keys map))))
