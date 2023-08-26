(ns sfsymus.leramir.types.timed-value
  (:require [sfsymus.leramir.rational :as r]))

(defrecord TimedValue [duration start attrs value])

(defn ->timed-value [duration start attrs value]
  {:pre [(and (r/rational? start) 
              (r/rational? duration))]}
  (assoc (->TimedValue duration start attrs value) :deps #{}))

(defn timed-value? [x]
  (= TimedValue (type x)))

(defn duration [tv]
  {:pre [timed-value? tv]}
  (:duration tv))

(defn start [tv]
  {:pre [(timed-value? tv)]}
  (:start tv))

(defn value [tv]
  {:pre [(timed-value? tv)]}
  (:value tv))

(defn end [tv]
  {:pre [(timed-value? tv)]}
  (r/+ (start tv) (duration tv)))

(defn register-deps [tv & paths]
  {:pre [(timed-value? tv)]}
  (update tv :deps (fnil (partial clojure.set/union) #{}) (set paths)))

(defn ->deps [tv]
  (:deps tv))

(defn attrs [tv]
  (:attrs tv))