(ns darkrogue.vismap)

; a vismap is a map of key -> keys that can see us
; basically an adjacency list...
(def empty-vismap {})

(defn mark-visible [vismap target source]
  "records in vismap that target is visible from source"
  (assoc vismap target (conj (get vismap target #{}) source)))

(defn get-observers [vismap target]
  "returns the set of items that can see this item"
  (get vismap target #{}))

(defn visible-from? [vismap target source]
  "true if target is visible from source"
  (contains? (get-observers vismap target) source))

(defn visible-from-any? [vismap target]
  "true if target is observed by at least one source"
  (boolean (seq (get-observers vismap target))))

(defn mark-targets-visible [vismap source targets]
  "records in vismap that the given list of targets are visible from source"
  (reduce #(mark-visible %1 %2 source) vismap targets))
