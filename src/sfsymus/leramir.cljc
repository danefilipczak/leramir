(ns sfsymus.leramir
  (:refer-clojure :exclude [ancestors])
  (:require [clojure.core.match :refer [match]]
            [clojure.set]
            [clojure.spec.alpha :as spec]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.data :as data]
            [sfsymus.leramir.rational :as r]
            [meander.epsilon :as m]
            [meander.strategy.epsilon]
            [sfsymus.leramir.types.timed-value :as tv]))

(defn spy [x]
  (println x)
  x)

(defn ->sorted-map [map]
  {:pre [(map? map)]}
  (apply sorted-map (apply concat map)))

(def special-keywords #{:heap :graft :chain :era})

(def uninheritable-attributes #{:scale :shift})

;; attributes that can modify time, and are therefore needed to express the overall structure of the piece.
(def temporal-attributes #{:shift :scale}) 

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

(def aliases {:+ :chain
              := :heap
              :<> :graft
              :- :era})

(defn parse-form [form]
  (match form
    [tag :guard keyword?
     attrs :guard map?
     & children]
    [(aliases tag tag) attrs children]
    
    [tag :guard keyword?
     & children]
    [(aliases tag tag) nil children]
    
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

(defn era? [form]
  (and (vector? form) (not (map-entry? form))))

(defn graft? [x]
  (and (era? x) (= (tag x) :graft)))

(defn chain? [x]
  (and (era? x) (= (tag x) :chain)))

(defn heap? [x]
  (and (era? x) (= (tag x) :heap)))

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

(def rewrite-grafts
  ;; this is cute but invalid, since grafts can only be rewritten if they have empty attrs
  (meander.strategy.epsilon/until
   =
   (meander.strategy.epsilon/rewrite
    [!before ... (m/pred graft? ?graft) . !after ...]
    (m/app
     (comp vec (partial apply concat))
     [[!before] ...
      (m/app children ?graft)
      . [!after] ...]))))

(tests
 ;; top level grafts are valid, and they have the same semantics as eras, ceteris peribus
 (rewrite-grafts [:graft 10 [:graft 10] 10]) := [:graft 10 10 10] 
 )

(def rewrite-grafts-bottom-up
  (meander.strategy.epsilon/bottom-up
   rewrite-grafts))

(tests
 (rewrite-grafts-bottom-up
  [:heap 1 2 3 4 [:graft 5 [:heap 6 [:graft {} 7 8]] 9] 10 [:graft 11] 12])
 := [:heap 1 2 3 4 5 [:heap 6 7 8] 9 10 11 12]
 
 )

(def effective-temporal-children (comp children rewrite-grafts))

(test
 (effective-temporal-children [:heap 1 2 3 [:graft 4 5 [:graft 4 5]]])
 )


(defn weight [x] 
  (if (graft? x)
    (r/integer (count (effective-temporal-children x)))
    r/one))

(tests
  (weight 1) := r/one 
  (weight [:graft 1 2 3 [:graft 4 [5]]]) := (r/integer 5)

  )

(defn update-single-val [m f & args]
  {:pre [(= 1 (count m))]}
  (let [[k v] (first m)]
    {k (apply f v args)}))

(defn register-dependencies [descendents self]
  (merge
   (apply update-single-val self tv/register-deps (keys descendents))
   descendents))

(comment
  (era->path-value-map [:chain 1 2 3])

  [:chain :backtrack :default :love 1 2 3]


  )

(defn era-get [era index]
  (nth (children era) index))

(defn era-get-in
  [era path]
  (reduce era-get era path))

(defn ->era
  ([tag attrs children]
   (into
    [(or tag :era)
     (or  attrs {})]
    (or children [])))
  ([tag attrs children meta]
   (with-meta (->era tag attrs children) meta)))

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

(declare era->path-value-map)

(defn path-value-map->era [path-value-map]
  ;; this is not recommend - prefer using the ast, a superset of the path-value-map that is structure preserving
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
     (if (era? form)
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

(defn contained? [start1 end1 start2 end2]
  ;; does span 1 completely contain span 2?
  (and (r/<= start1 start2)
       (r/>= end1 end2)))

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
  ;; ideally, this should be rewritten to take and return an ast.
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

(defn force-era [x]
  (if (era? x)
    x
    [x]))

(defn assoc-voices* [voice-tree-path form]
  (if (era? form)
    (let [era-duration (cond-> r/one
                         (chain? form) ;; or graft that is functioning as a chain! 
                         (r/* (r/integer (count (children form))))

                         :always (scale (attrs form)))
          era-start (shift era-duration r/zero (attrs form))
          era-end (r/+ era-start era-duration)
          overflow? (not (contained?
                          r/zero
                          r/one
                          era-start
                          era-end))
               ;; it may be useful to do a more sophisticated version of collision detection
          new-vtp (if overflow? (conj voice-tree-path 0) voice-tree-path)
          children (map-indexed
                    (fn [i child]
                      (apply assoc-voices*
                             (if (= :heap (tag form)) ;; or graft that is functioning as a heap
                               [(conj (pop new-vtp) (+ i (peek new-vtp)))
                                (force-era child)]
                               [new-vtp
                                child])))
                    (children form))]
      (->era
       (tag form)
       (assoc
        (attrs form)
        :voice new-vtp)
       children))
    form))

(defn assoc-voices [era]
  (assoc-voices* [:v 0] era))

(comment
  (assoc-voices
   [:=
    [:chain
     [:heap
      [1 2 3]
      [1 2 3]]
     :>
     3
     4]
    [:chain 1 2 3 4]
    3
    4]))

(defn empty-era? [era]
  (and (era? era) (every? nil? (children era))))

(def empty-graft? (every-pred graft? empty-era?))

(comment
  ;; our denomination code can be rewritten as follows:

  ;; percolate attributes
  ;; rewrite grafts -> our above approach is too naive because it denies the existence of structural attributes. 
  ;; what if a graft knew the closest non-graft ancestor, and iterpreted itself that way?
  ;; is this a fair assesment, that if we knew the ancestor we could compute the graft?

  ;; calculate voices -> we basically have this, but it also needs to respect grafts!
  ;; do the actual "denomination"
  ;; apply wings -> make a map of voice -> end -> tv. When exists, extend it by wing value.


  [:heap ;; the heap doesn't care how many kids it has
   [:graft
    ;; interpreted as heap
    ]]
  
  [:chain 
   1
   [:graft ;; interpreted as chain
    2
    3
    ]
   4]
  )

(defn handle-empty-grafts [era]
  (->era
   (tag era)
   (attrs era)
   (reduce
    (fn [acc curr]
      (if (empty-graft? curr)
        (vec (concat acc (take (count (children curr)) (repeat nil))))
        (conj acc curr)))
    []
    (children era))))

(defn handle-singleton-heaps [era]
    ;; if a heap has one element in it, and is not being used to modify time, it may be rewritten to that element
  (if (and (heap? era)
           (= 1 (count (children era)))
           (empty? (select-keys (attrs era) temporal-attributes)))
    (first (children era))
    era))

(defn query-by-attrs [pred form]
  (if-not (era? form)
    form
    (let [valid? (pred (attrs form))
          to-nil (fn [x] (when (or valid? (era? x)) x))
          new-era (->era
                   (tag form)
                   (if valid? (attrs form) (select-keys (attrs form) temporal-attributes))
                   (map (comp to-nil (partial query-by-attrs pred)) (children form)))]
      (when (or valid?
                (graft? new-era)
                (not (empty-era? new-era)))
        (-> new-era 
            ;; todo the only 'rewrite rule' we should have is to discard nil branches of the tree, retaining structure
            ;; grafts and eras will have to be treated differently, of course.
            handle-empty-grafts 
            handle-singleton-heaps)))))

(defn parse [era]
  (if (era? era)
    {:type ::era
     :tag (tag era)
     :attrs (attrs era)
     :children (mapv parse (children era))}
    {:type ::value
     :value era}))

(defn pathize* [path ast']
  (let [ast (assoc ast' :path path)]
    (case (:type ast)
      ::era (assoc 
             ast 
             :children 
             (map-indexed 
              (fn [i c] 
                (pathize* (conj path i) c))
              (:children ast)))
      ::value (assoc ast :path path))))

(defn pathize [ast]
  (pathize* [] ast)
  )

(defn recur-children [ast f] ;;maybe this receives a seq of f args to apply 
  (assoc ast :children (mapv f (:children ast))))

(defn effective-tagize* [effective-tag' {:keys [tag] :as ast}]
  (case (:type ast)
    ::era (let [effective-tag (if (= :graft tag)
                                effective-tag'
                                tag)]
            (-> ast
                (assoc :effective-tag effective-tag)
                (recur-children (partial effective-tagize* effective-tag))))
    ::value ast))

(defn effective-tagize [ast]
  (effective-tagize* :era ast) ;; defaults grafts to era
  )

(defn ast-weight [ast]
  (if (= :graft (:tag ast))
    (apply + (map ast-weight (:children ast)))
    1))

;; the interface that could be used here to minimize tree walking is:
;; transform era fn
;; transform value fn
;; initial data
;; compute child data, given current node, as a (potentially infinite) seq
(defn timeize* [{:keys [duration start]} {:keys [attrs] :as ast'}]
  (case (:type ast')
    ::era (let [duration (scale duration attrs)
                start (shift duration start attrs)
                ast (assoc ast' :duration duration :start start)
                accumulate-durations (fn [ast duration-fn acc-fn]
                                       (assoc 
                                        ast 
                                        :children
                                        (first 
                                         (reduce 
                                          (fn [[result prev-start prev-duration] curr]
                                            (let [new-duration (duration-fn curr) 
                                                  new-start (acc-fn prev-start prev-duration)]
                                              [(conj 
                                                result
                                                (timeize* 
                                                 {:start new-start :duration new-duration}
                                                 curr))
                                               new-start
                                               new-duration]))
                                          [[] start r/zero]
                                          (:children ast)))))]
            (case (:effective-tag ast) 
              :heap 
              ;; start start
              ;; duration duration
              (accumulate-durations 
               ast
               (constantly duration)
               (constantly start))
              :chain
              ;; start: previous start + previous duration
              ;; duration: weight * parent duration
              (accumulate-durations
               ast
               (fn [c] (r/*
                        (r/integer (ast-weight c))
                        duration))
               r/+)
              :era
              ;; start: previous start + previous duration 
              ;; duration: weight over total effective children
              (accumulate-durations
               ast
               (fn [c] (r/rational
                        (ast-weight c)
                        (reduce + (map ast-weight (:children ast)))))
               r/+)))
    ::value (assoc ast' :duration duration :start start)))
  
(defn timeize [ast]
  (timeize* {:start r/zero :duration r/one} ast))

(defn ast->path-value-map [ast]
  (let [->data (fn [x value-key]
                 {(:path x) (tv/->timed-value 
                             (:duration x) 
                             (:start x) 
                             (:percolated-attrs x) 
                             (value-key x))})]
    (if (= ::era (:type ast))
      (apply 
       merge 
       (->data ast :tag)
       (map ast->path-value-map (:children ast)))
      (->data ast :value))))

(defn percolate-attrs* [attrs ast]
  (if-not (= (:type ast) ::era)
    (assoc ast :percolated-attrs attrs)
    (let [new-attrs (merge-attrs (:attrs ast) attrs (:path ast))]
      (assoc 
       ast 
       :percolated-attrs 
       new-attrs
       :children
       (mapv (partial percolate-attrs* new-attrs) (:children ast))))))

(defn percolate-attrs [ast]
  (percolate-attrs* {} ast))

(defn era->path-value-map-via-ast [era]
  (-> era
      parse
      pathize
      effective-tagize
      timeize
      percolate-attrs
      ast->path-value-map))

(defn era->path-value-map [era]
  {:post [(path-value-map? %)]}
  ( era->path-value-map-via-ast era))

(comment
  
  (era->path-value-map-via-ast [:era {:color :green} 1 2 3 [:graft {:color :red :shift r/one} [4]]]) 
  )

(comment  

  ;; todo - we need to change the implementation of :<> so that it is respected in chains and heaps, not just eras

  ;; big todo for tomorrow:
  ;; extract out the functionality, and make it 'plug-and-play' able.

  ;; assignment of path -> preorder, takes previous path  ;; check
  ;; assignment of 'effective-tag' metadata, meaning the first non-graft ancestor of a graft ;;check
  ;; assignment of start and duration, taking grafts into special consideration by pimping them out to their effective tag ;;check
  ;; attr bubbling :> ;; sticking stuff under a different key. ;;check

  ;; this is parity -> todo check that everything renders in legacy renderer ;; check
  ;; remove references to the old stuff, rip it out

  ;; voice assignment 
  ;; dependency assignment 
  ;; wing value assignment :> make a map of voice->end->[value and path]. If exists, extend its duration by wing duration

  ;; todo make a generative spec, generate some truly large and unruly things, time parsing with both many-pass and single-pass method. 

  ;; keep an eye towards making these things composable -> see if a common interface could be available.  

  (query-by-attrs 
   :keep
   [:=
    [:graft 1 2 3]
    [:chain
     {:keep true}
     [:heap
      1
      2
      [1 2 3]
      [:era {:keep true} [1 2 3]]]
     :>
     3
     4]
    [:chain 1 2 3 4]
    3
    4]) 
  )

(defn era->voice-seq [era]
  (let [era-with-voice-annotations (assoc-voices era)
        all-voices #{}]
    (for [x all-voices]
      (query-by-attrs (comp (partial = x) :voice-tree-path) era)
      ))
  ;; a voice is, what exactly?
  ;; well, it has no parallel composition, by definition. 
  ;; it may only include single, rhythmically independent lines.
  ;; heaps are out of the question
  ;; chains, scales, etc are fine, and we probably don't want to collapse them

  [:chain 1 2 3 []]
  )

(comment
  ;; we want to preserve the intuition that an 'unvoiced' part can tie with voice 1
  [:=
   [nil]
   [nil]] ;; two different parts

  [:=
   [:+ [:+ nil 1] 2 :>]
   [nil]] ;; two parts, the first now has two voices. but which gets the wing?

  ;; # options:
  ;; the lastmost in the heap gets it - doesn't preserve the 'unvoiced part' intuition
  ;; the firstmost in the heap gets it - doesn't make sense because the firstmost isn't guaranteed to have terminated yet
  ;; whichever would naturally be assigned to the same voice, the same level of nesting, gets it - let's explore these implications  

  [:+ [:+ nil 1] 2 :>] ;; the 1 gets extended because the bird exists on the 'default' level
  [:+ [:+ nil 1 3] 2 :>] ;; the 2 gets extended because the bird has been pushed to heap 2

  ;; does this preserve the part / voice intuition? 
  [:=
   [:+
    [:=
     [1 2 3]
     [1 2 3]]
    [:> 2 3]]
   "part 2 stuff"]
  ;; yeah, basically

  ;; so how can we formalize this scheme?
  ;; parallel composition can be acheived in two ways
  ;; using the parallel operator, things retain their order
  ;; using a nested series operator, a new level of nesting is introduced. 

  ;; therefore, voices are hierarchical, not just simply ordered.
  ;; in order for the wing to take effect, you must match the voice at that hierarchy level if one exists.
  ;; otherwise, it can effect things lower in the hierarchy if it above them. maybe?

  ;; let's draw some graphs
  [:+ [:+ nil 1 3] 2 :>]

  :<> ;;graft
  :> ;;wing
  := ;;heap
  :+ ;;chain


  [:+]

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

  ;; pattern combination operators https://tidalcycles.org/docs/reference/pattern_structure
  ;; an interesting idea that can be replicated via functions
  ;; *n repeat a pattern - (take n (repeat x))
  ;; /n slow down a pattern - this is suggested to be our custom :mult op 
  )