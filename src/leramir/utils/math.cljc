(ns leramir.utils.math
  (:require [rational.core :as r]))

(defn map-range [value in-min in-max out-min out-max]
  (let [in-range (- in-max in-min)
        out-range (- out-max out-min)
        normalized-value (/ (- value in-min) in-range)]
    (+ out-min (* normalized-value out-range))))

(comment
  (float (map-range 5 0 10 0 20))

  )

(defn disjoint? [start1 end1 start2 end2]
  (or (r/<= end1 start2)
      (r/<= end2 start1)))

(def intersecting? (complement disjoint?))