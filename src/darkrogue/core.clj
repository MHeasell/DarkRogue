(ns darkrogue.core)

(require '[lanterna.screen :as s])
(use 'darkrogue.util)
(use 'darkrogue.graph)
(use 'darkrogue.coord)
(use 'darkrogue.grid)
(use 'darkrogue.worldgen)
(use 'darkrogue.universe)

; level drawing stuff

(defn get-glyph [sym]
  (cond
    (= :floor sym) \.
    (= :wall sym) \#
    (= :corridor sym) \~
    (= :empty sym) \space
    :else \space))

(defn get-display-item [grid coord]
  (let [area-coords (coords-in-rect (- (:x coord) 1) (- (:y coord) 1) 3 3)]
    (if (every? #(= :wall (get-cell grid %)) area-coords)
      :empty
      (get-cell grid coord))))

(defn draw-level [screen level coords]
  (let [screen-size (s/get-size screen)
        screen-width (second screen-size)
        screen-height (first screen-size)
        ]
    (dorun
      (map #(s/put-string screen (:x %) (:y %) (str (get-glyph (get-display-item level (add-coord % coords)))))
          (for [y (range screen-width)
                x (range screen-height)]
            (make-coord x y))))))

; player commands

(defn go-left [universe]
  (move-player universe (make-coord -1 0)))

(defn go-right [universe]
  (move-player universe (make-coord 1 0)))

(defn go-up [universe]
  (move-player universe (make-coord 0 -1)))

(defn go-down [universe]
  (move-player universe (make-coord 0 1)))


; main game initialization

(def input-command-mapping
  {:left go-left
   :right go-right
   :up go-up
   :down go-down
   \w go-up
   \a go-left
   \s go-down
   \d go-right})

(defn apply-input [universe input]
  (let [f (get input-command-mapping input)]
    (when f
      (f universe))))

(defn main-loop [screen]
  (draw-level screen (generate-world) (make-coord 0 0))
  (s/redraw screen)
  (if (= (s/get-key-blocking screen) \d)
    nil
    (recur screen)))

(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (s/put-string screen 0 0 "DarkRogue" {:styles #{:bold}})
                 (s/put-string screen 0 1 "A RogueLike game by Michael Heasell")
                 (s/put-string screen 0 2 "Press any key to generate level, D to exit.")
                 (s/redraw screen)
                 (s/get-key-blocking screen)
                 (main-loop screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text") :text
                      :else :auto)]
    (main screen-type)))
