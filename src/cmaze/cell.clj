(ns cmaze.cell
  (use cmaze.util))

(defrecord Cell [visited? valid-dirs open-dirs])

(defn create-cell
  "Creates an unvisited cell with some or no valid directions."
  ([] (Cell. false #{} #{}))
  ([valid-dirs] (Cell. false valid-dirs #{})))

(defn create-corner-cell [corner]
  "Creates a corner cell which has two valid directions."
  (case corner
    (:top-left) (create-cell #{:east :south})
    (:top-right) (create-cell #{:west :south})
    (:bottom-left) (create-cell #{:east :north})
    (:bottom-right) (create-cell #{:west :north})))

(defn create-wall-cell [side]
  "Creates a wall cell which has three valid directions."
  (case side
    (:top) (create-cell #{:west :south :east})
    (:left) (create-cell #{:north :south :east})
    (:bottom) (create-cell #{:west :north :east})
    (:right) (create-cell #{:north :south :west})))

(defn create-unbound-cell []
  "Creates a cell that has no invalid directions."
  (create-cell #{:north :east :south :west}))

(defn visited? [cell]
  "Determines whether or not a cell has been visisted."
  (true? (:visited? cell)))

(defn corner-cell? [index size]
  "Determines whether the index is a corner cell of #{top-left top-right bottom-left bottom-right} in a grid of size*size dimensions. Returns both whether or not the index is a corner cell, but also which corner of the grid it's on." (assert (not= 0 size))
  (cond (= index 0) [true :top-left]
        (= index (dec size)) [true :top-right]
        (= index (- (* size size) size)) [true :bottom-left]
        (= index (dec (* size size))) [true :bottom-right]
        :else [false]))

(defn wall-cell? [index size]
  "Determines whether the index is a wall cell of #{top left right bottom} in a grid of size*size dimensions. Returns both whether or not the index is a wall cell, but also which side of the grid it's on." (assert (not= 0 size))
  (let [contains-index (partial seq-contains index)
        top-row        (range 0 size)
        bottom-row     (range (- (* size size) size) (* size size))
        left-col       (take-while (partial > (* size size)) (iterate #(+ % size) 0))
        right-col      (take-while (partial > (* size size)) (iterate #(+ % size) (dec size)))]
    (cond (contains-index top-row) [true :top]
          (contains-index bottom-row) [true :bottom]
          (contains-index left-col) [true :left]
          (contains-index right-col) [true :right]
          :else [false])))

(def m-corner-cell? (memoize corner-cell?))
(def m-wall-cell? (memoize wall-cell?))