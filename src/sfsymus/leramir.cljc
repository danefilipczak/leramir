(ns sfsymus.leramir
  (:refer-clojure :exclude [ancestors])
  (:require [clojure.core.match :refer [match]]
            [clojure.set]
            [clojure.spec.alpha :as spec]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.data :as data]
            [sfsymus.leramir.rational :as r]
            [sfsymus.leramir.types.timed-value :as tv]
            [clojure.core.match :as m]))

(defn spy [x]
  (println x)
  x)

(defn ->sorted-map [map]
  {:pre [(map? map)]}
  (apply sorted-map (apply concat map)))

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

                         (sequential? x)
                         (cond
                           (contains? (methods denominate) (first x)) (first x) 
                           :else :era)
                         
                         :else :default)))

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
  (let [x (children x')]
    (if (empty? x)
      {}
      (let [divisions (reduce r/+ (map denomination x))
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
                        (when (sequential? curr) (attrs curr))
                        attrs
                        path)
                       curr))
                     r/zero])))
              [r/zero {} r/zero])
             second
             (into {}))))))

(defn update-single-val [m f & args]
  {:pre [(= 1 (count m))]}
  (let [[k v] (first m)]
    {k (apply f v args)}))

(defn register-dependencies [descendents self]
  (merge
   (apply update-single-val self tv/register-deps (keys descendents))
   descendents))

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
  (era->path-value-map [:chain 1 2 3])

  [:chain :backtrack :default :love 1 2 3]


  )

(defn era-get [era index]
  (nth (children era) index))

(defn era-get-in
  [era path]
  (reduce era-get era path))

(defn ->era [tag attrs children]
  (into
   [(or tag :era)
    (or  attrs {})]
   (or children [])))

(defn era-assoc [era index value]
  (->era
   (tag era)
   (attrs era)
   (assoc
    (children era)
    index
    value)))

(defn era-assoc-in 
  [m [k & ks] v]
  (if ks
    (era-assoc m k (era-assoc-in (era-get m k) ks v))
    (era-assoc m k v)))

(defn path? [x]
  (and (vector? x)
       (every? integer? x)))

(spec/def ::path-value-map (spec/map-of path? tv/timed-value?))

(defn path-value-map? [x]
  (spec/valid? ::path-value-map x))

(comment
  
  (path-value-map? (era->path-value-map [1 2 3]))
  
  [1 2 3 [2 3 [:tie 4]] [2 :>]]
  [[ 1 2 3 [:tie 4]] [4 2 3 4]]


  (get-in [1 3 12 [1 [1 2 3] 2]] [3 1 2]) 

  (get-in-era [[:heap 0 4]] [0 1])
  )

(defn era->path-value-map [x]
  {:post [( path-value-map? %)]}
  (denominate r/one r/zero [] {} x))

(defn path-value-map->era [path-value-map]
  {:pre [(path-value-map? path-value-map)]} 
  (first (reduce (fn [[acc attrs] [path tv]]
                   (if-not (contains? special-keywords (tv/value tv))
                     [(era-assoc-in acc path (tv/value tv)) attrs]
                     (let [era (->era
                                (tv/value tv)
                                (into {} (second (data/diff attrs (tv/attrs tv))))
                                [])]
                       [(if (seq path)
                          (era-assoc-in
                           acc
                           path
                           era)
                          era)
                        (merge attrs (tv/attrs tv))])))
                 [[] {}]
                 (->sorted-map path-value-map))))

(defn normalize-era* [era]
  (->era 
   (tag era)
   (attrs era)
   (children era)))

(defn normalize-era [era]
  (clojure.walk/postwalk 
   (fn [form]
     (if (and (sequential? form)
              (not (map-entry? form)))
       (normalize-era* form)
       form))
   era))

(tests
 "roundtrips"
 (let [era [:chain {} "te" 1 2 3 [:heap 1 [2 3]]]]
   (path-value-map->era
    (era->path-value-map
     era))
   :=
   (normalize-era era))
 
 "with attrs"
 (let [era [:chain {1 2} "te" 1 2 3 [:heap 1 [2 3]]]]
   (path-value-map->era
    (era->path-value-map
     era))
   :=
   (normalize-era era))
 
 "with non-inheritable attrs"
 (let [era [:chain {:scale (r/rational 1 2)} "te" 1 2 3 [:heap 1 [2 3]]]]
   (path-value-map->era
    (era->path-value-map
     era))
   :=
   (normalize-era era))
 
 "with complex nested attrs"
 (let [era [:chain 
            {1 2} 
            [:heap 
             {1 42
              2 4} 
             [:era 
              {1 2
               3 4}
              1]
             [:era
              {1 6
               3 6}
              1]]]]
   (path-value-map->era
    (era->path-value-map
     era))
   :=
   (normalize-era era))
 )

(def ^:private era->era (comp path-value-map->era era->path-value-map))

(defn roundtrips? [era]
  (every?
   true?
   (for [[path timed-value] (era->path-value-map era)
         :when (not (contains? special-keywords (tv/value timed-value)))]
     (= (era-get-in era path)
        (tv/value timed-value)))))

(tests
 (roundtrips? [:era 1 2 3]) := true 
 (roundtrips? [:era {} [:chain 1 2 3 [:heap [23] [45]]]]) := true 
 )

(defn disjoint? [start1 end1 start2 end2]
  (or (r/<= end1 start2)
      (r/<= end2 start1)))

(def intersecting? (complement disjoint?))

(comment 
  (intersecting?
   r/one
   (r/rational 2 1)
   (r/rational 1 2)
   (r/rational 2 1))
  
  [1 1 0 0]
  [0 0 1 1]
  )

(defn slice-pvm [pvm start duration]
  {:pre [(and 
          (path-value-map? pvm)
          (r/rational? start) 
          (r/rational? duration))]
   :post [(every? tv/timed-value? %)]}
  (let [end (r/+ start duration)
        slice (fn [[_path tv]]
                (let
                 [starts-within? (and (r/>= (tv/start tv) start)
                                      (r/< (tv/start tv) end))
                  ends-within? (and (r/> (tv/end tv) start)
                                    (r/<= (tv/end tv) end))
                  overlap? (intersecting?
                            start
                            end
                            (tv/start tv)
                            (tv/end tv))]
                  (clojure.core.match/match
                   [overlap? starts-within? ends-within?]
                    [false _ _]
                    nil

                    [_ true true] ;; :the-unmodified-thing
                    tv 

                    [_ false true] ;; :the-thing-truncated-as-a-tie-end
                    (tv/truncate-start tv start) 

                    [_ true false] ;; :the-thing-truncated-as-a-tie-start
                    (tv/truncate-end tv end)
                    
                    [_ false false];; :the-thing-truncated-as-a-tie-continue
                    (tv/truncate-both tv start end)
                    )))]
    (->> (->sorted-map pvm)
         (keep slice)
         (map (partial tv/translate (r/- r/zero start))))))

(comment 

  (slice-pvm 
   (era->path-value-map [:chain [1 2 3] [4 5 6]])
   r/one
   r/one)
  
  ;; we need a new data structure that represents eras as a tree that is constantly factored to itself.
  ;; do some compute on insert to make sure that everything is always expressed with a common denominator
  ;; then we need build to a query api where you can say things like, give me exactly what's happening here

  ;; concretely, what this would mean is that we choose a number that represents the smallest division of time, in nths of an era
  ;; every node in the tree could then become associated with a scalar that represents how many 'ticks' 

  ;; concrete step: test whether ties in musicxml work 'across voices' in the sense that a default voice will tie with voice 1
  [1 2 3]
  [nil true] ;; boolean ui
  )

(defn slice [era start duration]
  {:pre [(and (r/rational? start)
              (r/rational? duration))]}
  (path-value-map->era 
   (slice-pvm 
    (era->path-value-map era) 
    start 
    duration)))

(defn detect-circular-dependency? [path-value-map] ;; todo rewrite me : (
  (let [visited (atom #{})
        in-progress (atom #{})
        cycle (atom false)]

    (defn dfs [node]
      (swap! in-progress conj node)
      (doseq [dep (tv/->deps (path-value-map node))]
        (if (contains? @in-progress dep)
          (do (reset! cycle true)
              (throw (Exception. (str "Circular dependency found: " node " -> " dep))))
          (when (not (contains? @visited dep))
            (dfs dep))))
      (swap! in-progress disj node)
      (swap! visited conj node))

    (doseq [node (keys path-value-map)]
      (when (not (contains? @visited node))
        (dfs node)))

    @cycle))

(comment



  (detect-circular-dependency?
   (era->path-value-map [:heap 1 2 3]))
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
  
  [:chain [1 2 3] [:> 4 5]]








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