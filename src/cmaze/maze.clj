(ns cmaze.maze
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

(defn get-cell [maze x y]
  (let [cells (:cells maze)
        size (:size maze)
        index (+ x (* y size))]
    (when (> index (* size size)) (throw (IllegalArgumentException. (format "Index (%s, %s) exceeds size of maze." x y))))
    (nth cells index)))

(defn- link [cell dir]
  (assoc cell :open-dirs dir))

(defn- link-cell [maze cell idx]
  (let [{cells :cells size :size} maze
        {open-dirs :open-dirs} cell]
    (doseq [open-dirs open-dirs]
      (fn [open-dir]
        (case open-dir
          :east  (link (nth cells (inc idx)) :west)
          :west  (link (nth cells (dec idx)) :east)
          :north (link (nth cells (- idx size)) :south)
          :south (link (nth cells (+ idx size)) :north))))
    cell))

(defn link-cells [maze cells]
  (for [cell cells
        idx (range (count cells))]
    (link-cell maze cell idx)))

(defn traverse [maze f]
  (let [{cells :cells size :size} maze]
      (map (fn [cell] (f cell))
        cells)))

(defn binary-tree [cell]
  (let [valid-dirs (:valid-dirs cell)]
    (cond
      (and (contains? valid-dirs :north) (contains? valid-dirs :east)) (assoc cell :open-dirs #{:east})
      (contains? valid-dirs :north) (assoc cell :open-dirs #{:north})
      (contains? valid-dirs :east) (assoc cell :open-dirs #{:east})
      :else cell)))

;; sample usage: (link-cells maze (traverse maze binary-tree))