(ns darkrogue.util)

(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))
