(ns cmaze.png
  (:require [clojure.set :as set])
  (import java.io.File)
  (import java.awt.Color)
  (import java.awt.image.BufferedImage)
  (import javax.imageio.ImageIO))

(defn dir-to-line [x y cell-size dir]
  (case dir
    :east [(+ x cell-size) y (+ x cell-size) (+ y cell-size)]
    :west [x y x (+ y cell-size)]
    :north [x y (+ x cell-size) y]
    :south [x (+ y cell-size) (+ x cell-size) (+ y cell-size)]))

(defn cell-to-lines [size cell-size idx cell]
  (let [y (* (int (/ idx size)) cell-size)
        x (* (mod idx size) cell-size)
        closed-dirs (set/difference #{:north :east :south :west} (:open-dirs cell))]
    (map (partial dir-to-line x y cell-size) closed-dirs)))

(defn maze-to-lines [maze cell-size]
  (let [{cells :cells size :size} maze]
    (reduce concat (map-indexed (partial cell-to-lines size cell-size) cells))))

(defn draw
  "Renders a maze and saves it to a .png file"
  ([maze dest] (draw maze dest 5))
  ([maze dest cell-size]
   (let [{cells :cells size :size} maze
         total-size (* size size)
         lines-to-draw (maze-to-lines maze cell-size)
         bi (BufferedImage. (* cell-size total-size) (* cell-size total-size) BufferedImage/TYPE_INT_ARGB)
         g (.createGraphics bi)]
     (do
       (.setColor g Color/BLACK)
       (doseq [[x y x' y'] lines-to-draw]
         (.drawLine g x y x' y'))
       (ImageIO/write bi "png" (File. dest))))))