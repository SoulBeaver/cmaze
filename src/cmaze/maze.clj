(ns cmaze.maze
  (use cmaze.cell))

(defrecord Maze [cells])

(defn create-maze-cell [index size]
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
                   (range total-cells)))))