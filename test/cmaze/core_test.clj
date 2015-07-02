(ns cmaze.core-test
  (:use clojure.test cmaze.cell cmaze.maze)
  (:import (cmaze.cell Cell)))

(def unvisited-cell (create-cell))
(def visited-cell (Cell. true #{} #{}))

(def tl-cell (create-corner-cell :top-left))
(def tr-cell (create-corner-cell :top-right))
(def bl-cell (create-corner-cell :bottom-left))
(def br-cell (create-corner-cell :bottom-right))

(def l-cell (create-wall-cell :left))
(def t-cell (create-wall-cell :top))
(def r-cell (create-wall-cell :right))
(def b-cell (create-wall-cell :bottom))

(deftest cell
  (testing "creation" (is (= (Cell. false #{} #{}) (create-cell))))
  (testing "visitation" (is (= false (visited? unvisited-cell)))
    (is (= true (visited? visited-cell))))
  (testing "corner-cell" (is (= (Cell. false #{:east :south} #{}) tl-cell))
    (is (= (Cell. false #{:west :south} #{}) tr-cell))
    (is (= (Cell. false #{:east :north} #{}) bl-cell))
    (is (= (Cell. false #{:west :north} #{}) br-cell)))
  (testing "wall-cell" (is (= (Cell. false #{:north :south :east} #{}) l-cell))
    (is (= (Cell. false #{:west :south :east} #{}) t-cell))
    (is (= (Cell. false #{:north :south :west} #{}) r-cell))
    (is (= (Cell. false #{:west :north :east} #{}) b-cell)))
  (testing "unbound-cell" (is (= (Cell. false #{:east :south :west :north} #{})))))

(def tiny-maze (create-maze 1))
(def small-maze (create-maze 2))

(deftest maze-4x4
  (testing "creation"
    (is (= 1 (count (:cells tiny-maze))))
    (is (= 4 (count (:cells small-maze))))

    (is (empty? (filter #(visited? %1) tiny-maze)))
    (is (empty? (filter #(visited? %1) small-maze))))
  (testing "wall-cell-creation"
    (let [[_ top-side1] (wall-cell? 0 4)
          [_ top-side2] (wall-cell? 1 4)
          [_ top-side3] (wall-cell? 2 4)
          [_ top-side4] (wall-cell? 3 4)]
      (is (= :top top-side1))
      (is (= :top top-side2))
      (is (= :top top-side3))
      (is (= :top top-side4)))
    (let [[_ bottom-side1] (wall-cell? 12 4)
          [_ bottom-side2] (wall-cell? 13 4)
          [_ bottom-side3] (wall-cell? 14 4)
          [_ bottom-side4] (wall-cell? 15 4)]
      (is (= :bottom bottom-side1))
      (is (= :bottom bottom-side2))
      (is (= :bottom bottom-side3))
      (is (= :bottom bottom-side4)))
    (let [[_ left-side1] (wall-cell? 0 4)
          [_ left-side2] (wall-cell? 4 4)
          [_ left-side3] (wall-cell? 8 4)
          [_ left-side4] (wall-cell? 12 4)]
      (is (not= :left left-side1))
      (is (= :left left-side2))
      (is (= :left left-side3))
      (is (not= :left left-side4)))
    (let [[_ right-side1] (wall-cell? 3 4)
          [_ right-side2] (wall-cell? 7 4)
          [_ right-side3] (wall-cell? 11 4)
          [_ right-side4] (wall-cell? 15 4)]
      (is (not= :right right-side1))
      (is (= :right right-side2))
      (is (= :right right-side3))
      (is (not= :right right-side4))))
  (testing "corner-cell-creation"
    (let [[_ top-left] (corner-cell? 0 4)
          [_ top-right] (corner-cell? 3 4)
          [_ bottom-left] (corner-cell? 12 4)
          [_ bottom-right] (corner-cell? 15 4)]
      (is (= :top-left top-left))
      (is (= :top-right top-right))
      (is (= :bottom-left bottom-left))
      (is (= :bottom-right bottom-right)))))
