(ns symus.laurelmir.types.timed-value
  (:require [symus.laurelmir.rational :as r]))

(defn ->timed-value [duration start value]
  [duration start value])

(defn timed-value? [x]
  (and (vector? x)
       (= (count x) 3)
       (r/rational? (first x))
       (r/rational? (second x))))

(defn duration [tv]
  {:pre [timed-value? tv]}
  (first tv))

(defn start [tv]
  (second tv))

(defn value [tv]
  (last tv))

(defn end [tv]
  (r/+ (start tv) (duration tv)))