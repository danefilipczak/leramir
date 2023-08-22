(ns sfsymus.leramir.utils.colls)

(defn force-set [x]
  (cond 
    (set? x) x
    (coll? x) (set x)
    :else #{x}))