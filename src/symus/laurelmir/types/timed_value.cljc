(ns symus.laurelmir.types.timed-value
  (:require [symus.laurelmir.rational :as r]))

(defrecord TimedValue [duration start value])

(defn ->timed-value [duration start value]
  {:pre [(and (r/rational? start) 
              (r/rational? duration))]}
  (assoc (->TimedValue duration start value) :deps #{}))

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