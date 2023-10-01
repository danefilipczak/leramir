(ns leramir.protocols.pitch)


(defprotocol IPitch
  (discreet [x]) ;; an integer
  (continuous [x]) ;; a float
  )

(extend-protocol IPitch
  Number
  (discreet [x]
    (assert (>= 127 x 0) "Numbers used as pitches must be between 0 and 127")
    (int (Math/round (float x))))
  (continuous [x]
    (assert (>= 127 x 0) "Numbers used as pitches must be between 0 and 127")
    (float x)))


(defprotocol IPitchSet
  (pitch-class-set* [self])
  (pitch-set* [self]))

(extend-protocol IPitchSet
  Number
  (pitch-class-set* [self]
    #{(mod (discreet self) 12)})

  (pitch-set* [self]
    #{(discreet self)}))

(extend-protocol IPitchSet
  Object
  (pitch-class-set* [self]
    #{})

  (pitch-set* [self]
    #{}))

(defn maybe-discreet [x]
  (when (satisfies? IPitch x)
    (discreet x)))

(defn pitch-set [x]
  {:post [(set? %)]}
  (if (set? x)
    (into #{} (keep maybe-discreet x))
    (pitch-set* x)))

(defn pitch-class-set [x]
  {:post [(set? %)]}
  (if (set? x)
    (into #{} (keep #(some-> % maybe-discreet (mod 12)) x))
    (pitch-class-set* x)))

(comment 
  (pitch-set #{1 2 :ke})
  
  (pitch-class-set 23)
  )