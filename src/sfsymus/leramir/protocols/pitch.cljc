(ns sfsymus.leramir.protocols.pitch)


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