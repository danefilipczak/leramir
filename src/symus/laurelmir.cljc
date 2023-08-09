(ns symus.laurelmir
  (:refer-clojure :exclude [ancestors])
  (:require [clojure.core.match :refer [match]]
            [clojure.set]
            [clojure.spec.alpha :as spec]
            [hyperfiddle.rcf :refer [tests]]
            [symus.laurelmir.rational :as r]
            [symus.laurelmir.types.timed-value :as tv]))

(defn spy [x]
  (println x)
  x)

(def special-keywords #{:heap :graft :chain :era})

;; make attrs inheritable - keep in attrs metadata a map of the attr and the path from which it is descended
;; update parser to include scale, update viewer to include color attrs
(def uninheritable-attributes #{:scale :shift})

(defn merge-attrs [mine ancestors path]
  (with-meta 
    (merge 
     (select-keys
      ancestors
      (clojure.set/difference
       (set (keys ancestors))
       uninheritable-attributes)) 
     mine)
    (merge (meta ancestors)
           (zipmap (keys mine) (repeat path)))))

(tests
 (def x (merge-attrs 
         {:color :green} 
         (with-meta {:color :red} {:color []}) 
         [1]))
 x := {:color :green}
 (meta x) := {:color [1]}
 
 (merge-attrs
  {:color :blue}
  {(first uninheritable-attributes) :2}
  [])
 := 
 {:color :blue}
 )

(defmulti denominate (fn [_whole _start _path attrs x]
                       (cond
                         (set? x) ::set
                         (sequential? x)
                         (cond
                           (contains? (methods denominate) (first x)) (first x) 
                           :else :era))))

(defmethod denominate :default 
  [whole start path attrs x]
  (when (keyword? x)
    (assert (special-keywords x) (str "not special " x)))
  {path (tv/->timed-value whole start attrs x)})

(defn graft? [x]
  (and 
   (sequential? x)
   (= (first x) :graft)))

(defn parse-form [form]
  (match form
    [tag :guard keyword?
     attrs :guard map?
     & children]
    [tag attrs children]
    
    [tag :guard keyword?
     & children]
    [tag nil children]
    
    :else
    [:era nil form]))

(defn tag [form]
  (first (parse-form form)))

(defn attrs [form]
  (second (parse-form form)))

(defn children [form]
  ;; todo rename me
  (last (parse-form form)))

(tests
 ;; the leading keyword is a tag
 (tag [:era {}]) := :era

 ;; an empty tag is an anonymous era 
 (tag [1 2 3]) := :era 
 
 ;; at the second index lies an optional attrs map
 (attrs [:era {:color :green}]) := {:color :green}

 ;; you must have a tag to use attrs, they don't work for anonymous eras
 (attrs [{:color :green}]) := nil

 ;; in an anonymous era, everything is a child
 (children [{:color :green}]) := [{:color :green}] 

 (children [:era 1 2 3 4 5]) := [1 2 3 4 5])

(defn scale [whole attrs]
  (if-let [scale (:scale attrs)]
    (do (assert (r/rational? scale))
        (assert (r/pos? scale))
        (assert (r/rational? whole))
        (r/* whole scale))
    whole))

(defn shift [whole start attrs]
  (if-let [shift (:shift attrs)]
    (do (assert (r/rational? start)) 
        (assert (r/rational? shift))
        (r/+ start (r/* whole shift)))
    start))

(defn denomination [x] 
  ;; how many divisions am I worth?
  (if (graft? x)
    (r/rational (count (children x)) 1)
    r/one))

(defn denominate-era* [whole start path attrs x']
  (let [x (children x')
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
                   (merge-attrs 
                    (attrs curr)
                    attrs
                    path)
                   curr))
                 r/zero])))
          [r/zero {} r/zero])
         second
         (into {}))))

(defn update-single-val [m f & args]
  {:pre [(= 1 (count m))]}
  (let [[k v] (first m)]
    {k (apply f v args)}))

(defn register-dependencies [descendents self]
  (merge
   (apply update-single-val self tv/register-deps (keys descendents))
   descendents))

(defmethod denominate ::set
  [whole start path attrs x]
  (apply 
   merge
   (for [[i v] (zipmap (range) x)]
     (denominate whole start (conj path i) attrs v))))

(defmethod denominate :graft
  [whole' start' path ancestor-attrs x]
  (let [attrs (merge-attrs (attrs x) ancestor-attrs path)
        whole (scale whole' attrs)
        start (shift whole start' attrs)
        descendents (denominate-era* whole start path attrs x)
        self (denominate whole start path attrs :graft)]
    (register-dependencies descendents self)))

(defmethod denominate :era
  [whole' start' path ancestor-attrs x]
  (let [attrs (merge-attrs (attrs x) ancestor-attrs path)
        whole (scale whole' attrs)
        start (shift whole start' attrs)
        descendents (denominate-era* whole start path attrs x)
        self (denominate whole start path attrs :era)]
    (register-dependencies descendents self)))

(defmethod denominate :heap
 [whole' start' path ancestor-attrs form]
  (let [attrs (merge-attrs (attrs form) ancestor-attrs path)
        whole (scale whole' attrs)
        start (shift whole start' attrs)
        descendents (->> (children form)
                      (map-indexed
                       (fn [i x]
                         (denominate
                          whole
                          start
                          (conj path i)
                          attrs
                          x)))
                      (apply merge))
        self (denominate whole start path attrs :heap)]
    (register-dependencies descendents self)))

(defmethod denominate :chain
  [whole' start' path ancestor-attrs form]
  (let [attrs (merge-attrs (attrs form) ancestor-attrs path)
        whole (scale whole' attrs)
        start (shift whole start' attrs)
        descendents (->> (children form)
                         (map-indexed
                          (fn [i x]
                            (denominate
                             whole
                             (r/+ start (r/* whole (r/rational i 1)))
                             (conj path i)
                             attrs
                             x)))
                         (apply merge)) 
        self (denominate
              (r/* whole (r/rational (count (children form)) 1))
              start
              path
              attrs
              :chain)] 
    (register-dependencies descendents self)))

(comment
  (->path->timed-value [:chain 1 2 3])

  [:chain :backtrack :default :love 1 2 3]


  )

(defn era-get [era index]
  (nth (children era) index))

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
  (denominate r/one r/zero [] {} x))

(comment 
  
  (->path->timed-value 
   [:chain
    [1 2 3 [:graft [:heap 4 5] 5] :> 6 1]
    [1 2 3 [:graft [:heap [4 5] 1] 5] 6 :>]
    [1 2 3 [:graft [:heap 4 5] 5] 6 :>]])
  )

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