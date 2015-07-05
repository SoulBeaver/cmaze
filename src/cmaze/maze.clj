(ns cmaze.maze
  (require [clojure.set :as set])
  (use cmaze.cell))

(defrecord Maze [cells size])

(defn- create-maze-cell [index size]
  "Creates a cell depending on its location in the maze."
  (let [is-only-cell (= size 1)
        [is-corner-cell corner] (m-corner-cell? index size)
        [is-wall-cell side] (m-wall-cell? index size)]
    (cond (true? is-only-cell) (create-cell)
          (true? is-corner-cell) (create-corner-cell corner)
          (true? is-wall-cell) (create-wall-cell side)
          :else (create-unbound-cell))))

(defn create-maze [size]
  "Create a maze sizeXsize large."
  (Maze.
    (let [total-cells (* size size)]
      (map-indexed (fn [idx _] (create-maze-cell idx size))
        (range total-cells)))
    size))

(defn get-cell [maze idx dir]
  "Returns the neighboring cell of the cell at point idx. The direction defines which neighbor to return."
  (let [{cells :cells size :size} maze]
    (when (> idx (* size size)) (throw (IllegalArgumentException. (format "Index %s exceeds size of maze." idx))))
    (case dir
      :west (nth cells (dec idx))
      :east (nth cells (inc idx))
      :north (nth cells (- idx size))
      :south (nth cells (+ idx size)))))

(defn- is-open? [cell dir]
  (contains? (:open-dirs cell) dir))

(defn- link-cell [maze idx cell]
  (let [{valid-dirs :valid-dirs open-dirs :open-dirs} cell
        dirs-to-open (reduce (fn [dirs-to-open dir]
                               (case dir
                                 :west (if (is-open? (get-cell maze idx :west) :east) (conj dirs-to-open :west) dirs-to-open)
                                 :east (if (is-open? (get-cell maze idx :east) :west) (conj dirs-to-open :east) dirs-to-open)
                                 :north (if (is-open? (get-cell maze idx :north) :south) (conj dirs-to-open :north) dirs-to-open)
                                 :south (if (is-open? (get-cell maze idx :south) :north) (conj dirs-to-open :south) dirs-to-open)))
                       #{}
                       valid-dirs)]
    (assoc cell :open-dirs (set/union open-dirs dirs-to-open))))

(defn- link-cells [maze]
  (->Maze
    (map-indexed (partial link-cell maze) (:cells maze))
    (:size maze)))

(defn traverse [maze f]
  "Allows manipulation of a maze by iterating over each cell from top-left to bottom-right."
  (let [{cells :cells size :size} maze]
      (->Maze
        (map (fn [cell] (f cell)) cells)
        size)))

(defn generate-maze-layout [maze f]
  "Given function f, applies f to each cell in the maze starting at the top-left."
  (link-cells (traverse maze f)))

(defn binary-tree [cell]
  (let [valid-dirs (:valid-dirs cell)
        random-dir (rand-int 2)
        dir-to-open (if (= 0 random-dir) :east :north)]
    (cond
      (and (contains? valid-dirs :north) (contains? valid-dirs :east)) (assoc cell :open-dirs #{dir-to-open})
      (contains? valid-dirs :north) (assoc cell :open-dirs #{:north})
      (contains? valid-dirs :east) (assoc cell :open-dirs #{:east})
      :else cell)))

(defn- print-wall [pretty-cell dir]
  (case dir
    :west (str pretty-cell "<")
    :east (str pretty-cell ">")
    :north (str pretty-cell "^")
    :south (str pretty-cell "v")))

(defn- print-cell [cell]
  (let [open-dirs (:open-dirs cell)]
    (reduce print-wall "" (seq open-dirs))))

(defn pretty-print [maze]
  (let [{cells :cells size :size} maze
        printable-cells (map (fn [cell] (print-cell cell)) cells)]
    (dotimes [idx (* size size)]
      (when (= 0 (mod idx size)) (println))
      (print (nth printable-cells idx) " "))))