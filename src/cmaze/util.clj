(ns cmaze.util)

(defn seq-contains [value coll]
  (some #(= value %) coll))