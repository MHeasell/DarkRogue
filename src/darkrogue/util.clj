(ns darkrogue.util)

(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-vals [pred map]
  "removes keys from the map whose values satisfy pred"
  (select-keys map (remove #(pred (get map %)) (keys map))))
