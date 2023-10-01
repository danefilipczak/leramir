(ns leramir.types.timed-value
  (:require [leramir.rational :as r]
            [hyperfiddle.rcf :refer [tests]]))

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

(def tye-types #{:start :end :continue})

(defn >tie [tie tv]
  {:pre [(and (tye-types tie)
              (timed-value? tv))]}
  (assoc tv :tie tie))

(defn <tie [tv]
  {:pre [(timed-value? tv)]
   :post [(or (nil? %)
              (tye-types %))]}
  (:tie tv))

(def >tie-start (partial >tie :start))
(def >tie-end (partial >tie :end))
(def >tie-continue (partial >tie :continue))

;; [_ false true] ;; :the-thing-truncated-as-a-tie-end
(defn truncate-start [tv new-start]
  {:pre [(and (timed-value? tv)
              (r/rational? new-start)
              (r/> new-start (start tv)))]}
  (>tie-end 
   (->timed-value
    (r/+ 
     (duration tv)
     (r/- 
      (start tv) 
      new-start))
    new-start
    (attrs tv)
    (value tv))))

(tests

 (truncate-start
  (->timed-value
   r/one
   r/zero
   {}
   420)
  (r/rational 1 2))

 (truncate-start
  (->timed-value
   r/one
   r/zero
   {}
   420)
  (r/rational -2 1))
 :throws
 java.lang.AssertionError)

;; [_ false false];; :the-thing-truncated-as-a-tie-continue
(defn truncate-both [tv new-start end]
  {:pre [(and (timed-value? tv)
              (r/rational? new-start)
              (r/rational? end)
              (r/> new-start (start tv))
              #_(r/< end (end tv)))]}
  (-> (->timed-value
       (r/- end new-start)
       new-start
       (attrs tv)
       (value tv)) 
      (>tie-continue)))

(comment 
  
  (truncate-both 
   (->timed-value 
    r/one 
    r/zero
    {}
    420)
   (r/rational 1 2)
   (r/rational 2 3))
  )

;; [_ true false] ;; :the-thing-truncated-as-a-tie-start
(defn truncate-end [tv new-end]
  {:pre [(and (timed-value? tv)
              (r/rational? new-end)
              (r/< new-end (end tv)))]}
  (-> (->timed-value
       (r/- new-end (start tv))
       (start tv)
       (attrs tv)
       (value tv))
      (>tie-start)))

(tests

 (truncate-end
  (->timed-value
   r/one
   r/zero
   {}
   420)
  (r/rational 1 2)) 
 
 (truncate-end
  (->timed-value
   r/one
   r/zero
   {}
   420)
  (r/rational 2 1)) 
 :throws 
 java.lang.AssertionError
 
 
 )

(defn translate [offset tv]
  (->timed-value 
   (duration tv)
   (r/+ offset (start tv))
   (attrs tv)
   (value tv)))