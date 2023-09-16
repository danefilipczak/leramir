(ns sfsymus.leramir.rational
  (:refer-clojure :exclude [rational? numerator denominator + - * / *' +' - -' pos? neg? zero? >= > <= <])
  (:require [hyperfiddle.rcf :refer [tests]]
            [sfsymus.leramir.rational :as r]))

(def sum clojure.core/+)
(def product clojure.core/*)
(def difference clojure.core/-)
(def quotient clojure.core//)

(defrecord Ratio [numerator denominator])

(defn numerator [rational]
  (:numerator rational))

(defn denominator [rational]
  (:denominator rational))

#_(defmethod clojure.core/print-method Ratio [v ^java.io.Writer w]
  (clojure.core/print-method 
   (symbol (str "r" (numerator v) "/" (denominator v))) 
   w)) ;;as a symbol, no r

#_(defmethod clojure.pprint/simple-dispatch
  Ratio
  [r] (print (symbol (str "r" (numerator r) "/" (denominator r)))))

(defn rational? [x]
  (= (type x) Ratio))
(comment 

  (symbol "a")
  (type Ratio) 
  (type (rational 1 2))
  (type (rational 1 2))
  )

(defn rational [numerator denominator]
  (->Ratio numerator denominator))

(defn reciprocal [rat]
  (rational (denominator rat) (numerator rat)))

(defn greatest-common-divisor [a b]
  (if (clojure.core/zero? b)
    a
    (recur b (mod a b))))

(defn least-common-multiple [a b]
  (if (or (clojure.core/zero? a) (clojure.core/zero? b))
    0
    (quotient 
     (product a b) 
     (greatest-common-divisor a b))))

(defn convert-denominator [new-denominator r]
  (r/rational
   (product 
    (numerator r) 
    (quotient new-denominator (denominator r)))
   new-denominator))

(defn normalize [rat]
  (let [gcd (greatest-common-divisor (numerator rat) (denominator rat))]
    (rational
     (quotient (numerator rat) gcd) 
     (quotient (denominator rat) gcd))))

(defn *' [rata ratb]
  (rational 
   (product (numerator rata) (numerator ratb))
   (product (denominator rata) (denominator ratb))))

(defn * [rata ratb]
  (normalize (*' rata ratb)))

(defn / [rata ratb]
  (* rata (reciprocal ratb)))

(defn +' [rata ratb]
  (rational
   (sum 
    (product (numerator rata) (denominator ratb)) 
    (product (numerator ratb) (denominator rata)))
   (product 
    (denominator ratb) 
    (denominator rata))))

(defn + [rata ratb]
  (normalize (+' rata ratb)))

(defn -' [rata ratb]
  (rational
   (difference
    (product (numerator rata) (denominator ratb))
    (product (numerator ratb) (denominator rata)))
   (product
    (denominator ratb)
    (denominator rata))))

(defn - [rata ratb]
  (normalize (-' rata ratb)))

(defn realize [rat]
  (quotient (numerator rat) (denominator rat)))

(def one (rational 1 1))
(def zero (rational 0 1))

(defn zero? [r]
  (clojure.core/zero? (numerator r)))

(defn integer [i]
  (rational i 1))

(defn ^:private neg?* [r]
  (not= 
   (clojure.core/neg? (numerator r))
   (clojure.core/neg? (denominator r))))

(def ^:private pos?* (complement neg?*))

(defn neg? [r]
  (if (zero? r)
    false
    (neg?* r)))

(defn pos? [r]
  (if (zero? r)
    false
    (pos?* r)))

(tests
 (neg? one) := false
 (neg? zero) := false
 (neg? (rational 1 -2)) := true 

 (zero? zero) := true
 (pos? zero) := false
 (neg? zero) := false
 
 )

(defn unify-denominators [& rs]
  (let [d (reduce least-common-multiple (map denominator rs))]
    (map (partial convert-denominator d) rs)))

(defn make-comparator [compare-fn]
  (fn [& rs]
    (apply compare-fn (map numerator (apply unify-denominators rs)))))

(def >= (make-comparator clojure.core/>=))
(def > (make-comparator clojure.core/>))
(def <= (make-comparator clojure.core/<=))
(def < (make-comparator clojure.core/<))

(tests 
 (>= (rational 3 2) (rational 2 2) (rational 2 2) (rational 1 2) (rational 1 2)) := true
 (>= (rational 2 2) (rational 2 2) (rational 3 2) (rational 1 2) (rational 1 2)) := false

 (> (rational 1 3) (rational 2 3)) := false 
 (> (rational 1 3) (rational 1 3)) := false
 (> (rational 1 3) (rational 1 6)) := true
 )

(comment
  
  
  (unify-denominators 
   (rational 1 2) 
   (rational 1 4)
   (rational 1 3)
   (rational 1 13))
  )