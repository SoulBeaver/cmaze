(ns cmaze.png
  (:require [clojure.set :as set])
  (import java.io.File)
  (import java.awt.Color)
  (import java.awt.image.BufferedImage)
  (import javax.imageio.ImageIO))

(defn draw
  "Renders a maze and saves it to a .png file"
  ([maze dest] (draw maze dest 5))
  ([maze dest cell-size]
    (let [{cells :cells size :size} maze
          total-size (* size size)
          bi (BufferedImage. (* cell-size total-size) (* cell-size total-size) BufferedImage/TYPE_INT_ARGB)
          g (.createGraphics bi)]
      (do
        (.setColor g Color/BLACK)
        (dotimes [row size]
          (dotimes [col size]
            (let [idx (+ col (* row size))
                  cell (nth cells idx)
                  closed-dirs (set/difference #{:north :east :south :west} (:open-dirs cell))
                  x (* col cell-size)
                  y (* row cell-size)]
              (doseq [closed-dir closed-dirs]
                (case closed-dir
                  :east (.drawLine g (+ x cell-size) y (+ x cell-size) (+ y cell-size))
                  :west (.drawLine g x y x (+ y cell-size))
                  :north (.drawLine g x y (+ x cell-size) y)
                  :south (.drawLine g x (+ y cell-size) (+ x cell-size) (+ y cell-size)))))))
        (ImageIO/write bi "png" (File. dest))))))