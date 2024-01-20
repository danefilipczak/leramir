(ns leramir.seam
  (:require [leramir.value :as v]))

(def long-syntax :seam)
(def short-syntax :->)

(defn seam? [x]
  (and (vector? x)
       (#{long-syntax short-syntax} (first x))))

(defn value [seam]
  (assert (= 2 (count seam)) "Invalid seam")
  (let [x (second seam)]
    (assert (v/value? x))
    x))