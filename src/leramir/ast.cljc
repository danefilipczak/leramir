(ns leramir.ast
  (:require [leramir.era]
            [leramir.rational :as r]
            [leramir.types.timed-value :as tv]
            [hyperfiddle.rcf :refer [tests]]))

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
 {:color :blue})

(defn contained? [start1 end1 start2 end2]
  ;; does span 1 completely contain span 2?
  (and (r/<= start1 start2)
       (r/>= end1 end2)))

(defn parse [era]
  (if (leramir.era/era? era)
    {:type ::era
     :tag (leramir.era/tag era)
     :attrs (leramir.era/attrs era)
     :children (mapv parse (leramir.era/children era))}
    {:type ::value
     :value era}))

(defn ast-weight [ast]
  (if (= :graft (:tag ast))
    (apply + (map ast-weight (:children ast)))
    1))

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
  (pathize* [] ast))

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


;; the interface that could be used here to minimize tree walking is:
;; transform era fn
;; transform value fn
;; initial data
;; compute child data, given current node, as a (potentially infinite) seq
(defn timeize* [{:keys [duration start]} {:keys [attrs] :as ast'}]
  (case (:type ast')
    ::era (let [bounds [start (r/+ start duration)]
                total-weight (reduce + (map ast-weight (:children ast')))
                chain? (= (:tag ast') :chain)
                duration' (scale duration attrs)
                start' (shift duration' start attrs)
                ast (assoc
                     ast'
                     :duration (cond-> duration'
                                 chain?
                                 (r/* (r/integer total-weight)))
                     :start start')
                accumulate-durations (fn [ast duration-fn acc-fn]
                                       (let [children (first
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
                                                        [[] start' r/zero]
                                                        (:children ast)))]
                                         (merge ast {:bounds bounds :children children})))]
            (case (:effective-tag ast) 
              :heap 
              ;; start start
              ;; duration duration
              (accumulate-durations 
               ast
               (constantly duration')
               (constantly start'))
              :chain
              ;; start: previous start + previous duration
              ;; duration: weight * parent duration
              (accumulate-durations
               ast
               (fn [c] (r/*
                        (r/integer (ast-weight c))
                        duration'))
               r/+)
              :era
              ;; start: previous start + previous duration 
              ;; duration: weight over total effective children
              (accumulate-durations
               ast
               (fn [c] (r/* (r/rational
                             (ast-weight c)
                             total-weight)
                            duration'))
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

(defn voiceize* [voice-tree-path ast]
  (if (= ::era (:type ast))
    (let [[bound-start bound-end] (:bounds ast)
          overflow? (not (contained?
                          bound-start
                          bound-end
                          (:start ast)
                          (r/+ (:start ast) (:duration ast))))
               ;; it may be useful to do a more sophisticated version of collision detection
          new-vtp (if overflow? (conj voice-tree-path 0) voice-tree-path)
          children (vec (map-indexed
                         (fn [i child]
                           (apply voiceize*
                                  (if (= :heap (:effective-tag ast)) ;; this might be wrong for grafts. The easiest way to validate will be to build the editor.
                                    [(conj (pop new-vtp) (+ i (peek new-vtp)))
                                     child]
                                    [new-vtp
                                     child])))
                         (:children ast)))]
      (assoc ast :voice voice-tree-path :children children))
    (assoc ast :voice voice-tree-path)))

(defn voiceize [era]
  (voiceize* [:v 0] era))

(defn standard-interpretation [era]
  (-> era
      parse
      pathize
      effective-tagize
      timeize
      percolate-attrs
      voiceize))

(defn ast-path [path]
  (vec (interleave (repeat :children) path)))

(comment
  (def ast (standard-interpretation [:heap
                                     [1 [:chain 2 2] 3]
                                     [1 2 3]]))

  (era->path-value-map-via-ast [:era {:color :green} 1 2 3 [:graft {:color :red :shift r/one} [4]]]))

(defn era->path-value-map-via-ast [era]
  {:post [(leramir.era/path-value-map? %)]}
  (-> era 
      standard-interpretation
      ast->path-value-map))

(defn ->voice->end->path [ast] ;;todo memoize
  (case (:type ast)
    ::era (apply 
           (partial merge-with merge)
           (map 
            ->voice->end->path 
            (reverse (:children ast)))) ;; left to right in order to overwrite wings - does this work in every case?
    ::value {(:voice ast) 
             {(r/+ (:start ast) (:duration ast)) 
              (:path ast)}}))

(defn distribute-wings* [ast current-node]
  (let [voice->end->path (->voice->end->path ast)]
    (case (:type current-node)
      ::era (reduce 
             distribute-wings* 
             ast
             (:children current-node))
      ::value (if (= :> (:value current-node))
                (if-let [path (get-in 
                               voice->end->path 
                               [(:voice current-node) (:start current-node)])]
                  (update-in 
                   ast 
                   (conj 
                    (ast-path path) :duration) 
                   (partial r/+ (:duration current-node)))
                  ast)
                ast))))

(defn distribute-wings [ast]
  (distribute-wings* ast ast))

(comment

  (standard-interpretation [1 [5]])

  (->voice->end->path
   (distribute-wings
    (standard-interpretation [1 2 3 4 [5] :>])))

  ;; todo - we need to change the implementation of :<> so that it is respected in chains and heaps, not just eras

  ;; big todo for tomorrow:
  ;; extract out the functionality, and make it 'plug-and-play' able.

  ;; assignment of path -> preorder, takes previous path  ;; check
  ;; assignment of 'effective-tag' metadata, meaning the first non-graft ancestor of a graft ;;check
  ;; assignment of start and duration, taking grafts into special consideration by pimping them out to their effective tag ;;check
  ;; attr bubbling :> ;; sticking stuff under a different key. ;;check

  ;; this is parity -> todo check that everything renders in legacy renderer ;; check
  ;; remove references to the old stuff, rip it out

  ;; voice assignment ;; czek
  ;; dependency assignment ;; we don't really need it for now. we know what the model is - come back to this when it's actually useful for effects, etc.
  ;; wing value assignment :> make a map of voice->end->path. If exists, extend its duration by wing duration

  ;; todo make a generative spec, generate some truly large and unruly things, time parsing with both many-pass and single-pass method. 

  ;; keep an eye towards making these things composable -> see if a common interface could be available.  
  )