(ns symus.laurelmir
  (:require [symus.laurelmir.rational :as r]
            [symus.laurelmir.types.timed-value :as tv]
            [clojure.spec.alpha :as spec]
            [clojure.spec.alpha :as s]))

(defn spy [x]
  (println x)
  x)

(def special-keywords #{:heap :graft :scale :chain :era})

(defmulti denominate (fn [_whole _start _path x]
                       (cond
                         (set? x) ::set
                         (sequential? x)
                         (cond
                           (contains? (methods denominate) (first x)) (first x) 
                           :else :era))))

(defmethod denominate :default 
  [whole start path x]
  (when (keyword? x)
    (assert (special-keywords x) (str "not special " x)))
  {path (tv/->timed-value whole start x)})

(defn graft? [x]
  (and 
   (sequential? x)
   (= (first x) :graft)))

(defn drop-syntax [form]
  ;; this will need to become smarter as we offer compound syntax
  (drop-while keyword? form))

(defn denomination [x] 
  ;; how many divisions am I worth?
  (if (graft? x)
    (r/rational (count (drop-syntax x)) 1)
    r/one))

(defn denominate-era* [whole start path x']
  (let [x (drop-syntax x')
        divisions (reduce r/+ (map denomination x))
        per-value (r// whole divisions)
        end (r/+ start whole)]
    (->> x
         (interleave (range))
         (partition 2)
         reverse
         (reduce
          (fn [[i acc additional] [index curr]]
            (let [duration (r/+ (r/* per-value (denomination curr))
                                (r/* per-value additional))
                  start' (r/- end
                              (r/* per-value
                                   (r/+ i (denomination curr))))
                  next-i (r/+ i (denomination curr))]
              (if (= curr :>)
                [next-i
                 acc
                 (r/+ additional r/one)]
                [next-i
                 (merge
                  acc
                  (denominate
                   duration
                   start'
                   (conj path index)
                   curr))
                 r/zero])))
          [r/zero {} r/zero])
         second
         (into {}))))

(defn update-single-val [m f & args]
  {:pre [(= 1 (count m))]}
  (let [[k v] (first m)]
    {k (apply f v args)}))

(defn register-dependencies [children self]
  (merge
   (apply update-single-val self tv/register-deps (keys children))
   children))

(defmethod denominate ::set
  [whole start path x]
  (apply 
   merge
   (for [[i v] (zipmap (range) x)]
     (denominate whole start (conj path i) v))))

(defmethod denominate :graft
  [whole start path x]
  (let [children (denominate-era* whole start path x)
        self (denominate whole start path :graft)]
    (register-dependencies children self)))

(defmethod denominate :era
  [whole start path x]
  (let [children (denominate-era* whole start path x)
        self (denominate whole start path :era)]
    (register-dependencies children self)))

(defmethod denominate :heap
 [whole start path form]
  (let [children (->> (drop-syntax form)
                      (map-indexed
                       (fn [i x]
                         (denominate
                          whole
                          start
                          (conj path i)
                          x)))
                      (apply merge))
        self (denominate whole start path :heap)]
    (register-dependencies children self)))

(defmethod denominate :chain
  [whole start path form]
  (let [children (->> (drop-syntax form)
                      (map-indexed
                       (fn [i x]
                         (denominate
                          whole
                          (r/+ start (r/* whole (r/rational i 1)))
                          (conj path i)
                          x)))
                      (apply merge)) 
        self (denominate
              (r/* whole (r/rational (count (drop-syntax form)) 1))
              start
              path
              :chain)] 
    (register-dependencies children self)))

(comment
  (->path->timed-value [:chain 1 2 3])

  [:chain :backtrack :default :love 1 2 3]


  )

(defn era-get [era index]
  (nth (drop-syntax era) index))

(defn era-get-in
  [p ks]
  (reduce era-get p ks))

(defn path? [x]
  (and (vector? x)
       (every? integer? x)))

(defn path->timed-value? [x]
  (spec/valid? (spec/map-of path? tv/timed-value?) x))

(comment
  
  (path->timed-value? (->path->timed-value [1 2 3]))
  
  [1 2 3 [2 3 [:tie 4]] [2 :>]]
  [[ 1 2 3 [:tie 4]] [4 2 3 4]]


  (get-in [1 3 12 [1 [1 2 3] 2]] [3 1 2]) 

  (get-in-era [[:heap 0 4]] [0 1])
  )

(defn ->path->timed-value [x]
  (denominate r/one r/zero [] x))

(defn roundtrips? [era]
  (every?
   true?
   (for [[path timed-value] (->path->timed-value era)]
     (= (era-get-in era path)
        (tv/value timed-value)))))

;; todo tests for roundtrips?

(defn detect-circular-dependency? [p->tv] ;; todo rewrite me : (
  (let [visited (atom #{})
        in-progress (atom #{})
        cycle (atom false)]

    (defn dfs [node]
      (swap! in-progress conj node)
      (doseq [dep (tv/->deps (p->tv node))]
        (if (contains? @in-progress dep)
          (do (reset! cycle true)
              (throw (Exception. (str "Circular dependency found: " node " -> " dep))))
          (when (not (contains? @visited dep))
            (dfs dep))))
      (swap! in-progress disj node)
      (swap! visited conj node))

    (doseq [node (keys p->tv)]
      (when (not (contains? @visited node))
        (dfs node)))

    @cycle))

(comment



  (detect-circular-dependency?
   (->path->timed-value [:heap 1 2 3]))
  )


(comment 
  (roundtrips? [1 2 3 [:heap 1 2 [:heap 1 2 3]]])
  
  (parse [1 2 3 [:graft 3]])


  ;; data structure : path to timed value at path.
  

  

  ;; todo implement me
  (parse [:graft 3])
  
  (parse [[:a] [:> 1 2 3]]) ;; this is incorrect - it should extend the a

  ;; :> is a no-op as a child of a stack ??
  
  )

(comment 
  (defn graft [& args]
    (into [:graft] args))

  (defn stacatto [& args]
    (apply graft :stacatto args))

  (stacatto 1 2 3)

  ;;;; protocol thinking
  (defprotocol IRationalize
    (content) ;; return what's in you as a sorted, rationalized set
    (denomination) ;; returns how many 'counts' you're worth - usually this will be 1, but may be more in the case of grafts
    )


  ;; heap


  ;; scale

  (def x [[:a (r/rational 0 4) (r/rational 1 4)]
          [:b (r/rational 1 4) (r/rational 1 4)]
          [:c (r/rational 2 4) (r/rational 1 4)]
          [:d (r/rational 3 4) (r/rational 1 4)]])

  ;; apply [:scale 3 ...]

  ;; scaling is just multiplication
  (mapv
   (fn [[thing start duration]]
     [thing
      (r/* start (r/rational 3 1))
      (r/* duration (r/rational 3 1))])
   x)

  ;; chaining is addition of the start by index in the chain 
  (mapv
   (fn [[thing start duration]]
     [thing
      (r/+ start (r/rational 1 1))
      duration])
   x)

  ;; heap means setting start to 0 and duration to 1

  ;; graft means a different way of calculating "content"

  ;; when should :> be calculated?
  ;; ideally this would occur as quickly in the interpretation stack as possible

  ;; possible rule:
  ;; if your ending is my beginning, update your duration to add my duration

  [1
   [:stack
    [3 4]
    [13 14]
    [23 24]]
   :>]

  ;; in the above example, 4 14 and 24 all end at :>'s beginning, therefore their durations should be extended by the length of :>

  ;; this isn't quite right

  ;; another possible rule:
  ;; look at the thing before me, and add the duration of :> to its duration.

  [1
   [:stack
    [3 4]
    [13 14]
    [23 24]]
   :>]
  
  ;; in the above example, the entire :stack will now take, in this case, twice as long as it did

  [1
   [:graft
    [3 4]
    [13 14]
    [23 24]]
   :>]
  
  ;; in the above, there are five divisions of the pattern, with [23 24] getting two beats
  ;; 








  [:graft 3 :> :>]
  ;; you need either a start and a duration, or a start and an end



  ;;;;;;




  [:chain :mult 3 [4 5 6]]

  (drop-while keyword?)


  ;; pattern combination operators https://tidalcycles.org/docs/reference/pattern_structure
  ;; an interesting idea that can be replicated via functions
  ;; *n repeat a pattern - (take n (repeat x))
  ;; /n slow down a pattern - this is suggested to be our custom :mult op 
  )