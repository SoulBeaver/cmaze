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
          bi (BufferedImage. (* cell-size size) (* cell-size size) BufferedImage/TYPE_INT_ARGB)
          g (.createGraphics bi)]
      (do
        (.setColor g Color/BLACK)
        (.drawLine g 0 0 (* cell-size total-size) 0)
        (.drawLine g 0 0 0 (* cell-size total-size))
        (.drawLine g (* cell-size total-size) 0 (* cell-size total-size) (* cell-size total-size))
        (.drawLine g 0 (* cell-size total-size) (* cell-size total-size) (* cell-size total-size))
        ;(dotimes [x size]
        ;  (dotimes [y size]
        ;    (let [idx (+ y (* x size))
        ;          cell (nth cells idx)
        ;          closed-dirs (set/difference #{:north :east :south :west} (:open-dirs cell))]
        ;      (doseq [open-dir closed-dirs]
        ;        (case open-dir
        ;          :east (.drawLine g (* x size) (* y size) (+ (* x size) cell-size) (* y size))
        ;          :west (.drawLine g (* x size) (* y size) (- (* x size) cell-size) (* y size))
        ;          :north (.drawLine g (* x size) (* y size) (* x size) (- (* y size) cell-size))
        ;          :south (.drawLine g (* x size) (* y size) (* x size) (+ (* y size) cell-size)))))))
        (ImageIO/write bi "png" (File. dest))))))