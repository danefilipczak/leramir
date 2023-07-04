(ns symus.laurelmir.utils.math)

(defn map-range [value in-min in-max out-min out-max]
  (let [in-range (- in-max in-min)
        out-range (- out-max out-min)
        normalized-value (/ (- value in-min) in-range)]
    (+ out-min (* normalized-value out-range))))

(comment
  (float (map-range 5 0 10 0 20))

  )