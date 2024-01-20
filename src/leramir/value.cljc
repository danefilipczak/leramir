(ns leramir.value)

(defn value? [x]
  (or (set? x)
      (not (coll? x))))