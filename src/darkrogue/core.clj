(ns darkrogue.core)

(require '[lanterna.screen :as s])
(use 'darkrogue.util)
(use 'darkrogue.graph)
(use 'darkrogue.coord)
(use 'darkrogue.grid)
(use 'darkrogue.worldgen)

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

; main game initialization

(defn input-to-command [input]
  (cond
    (= input :left) :walk-left
    (= input :right) :walk-right
    (= input :up) :walk-up
    (= input :down) :walk-down))

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
