(ns leramir.viewer
  (:require [dali.syntax :as dali]
            [dali.io :as io]
            [leramir.era :as l]
            [nextjournal.clerk :as clerk]
            [leramir.types.timed-value :as tv]
            [leramir.protocols.pitch :as pitch]
            [rational.core :as r]
            [leramir.core :as core]
            [leramir.utils.math :as utils.math]))

(defn spy [x]
  (println x)
  x)

(comment
  (clerk/serve! {:browse true})
  
  )

(defn piano-roll [width height path-value-map]
  (let [pitches (sort (mapcat (comp pitch/pitch-set tv/value) (vals path-value-map)))
        min-pitch (first pitches)
        max-pitch (last pitches)
        stave (reverse (range min-pitch (inc max-pitch))) 
        height-per-stave (float (/ height (count stave)))
        last-end (last (sort (map (comp r/realize tv/end) (vals path-value-map)))) 
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        nn->i (zipmap stave (range))
        nn->height (fn [nn]
                     (assert (int? nn))
                     (* (nn->i nn) height-per-stave))
        ] 
    (concat (for [[i nn] (partition 2 (interleave (range) stave))]
              [:rect
               {:stroke :gainsboro
                :stroke-width 0.5
                :fill (if (contains?
                           #{0 2 4 5 7 9 11}
                           (int (mod nn 12)))
                        :white
                        :whitesmoke)
                :x 0
                :y (nn->height nn)
                :width width
                :height height-per-stave}])
            (for [tv (vals path-value-map) 
                  :let [pitch-set (pitch/pitch-set (tv/value tv))
                        color (:color (tv/attrs tv))]
                  nn pitch-set
                  :when nn]
              [:rect
               {:stroke :gainsboro 
                :stroke-width 0.5 
                :fill (or color :black)
                :x (rescale-tv-time (tv/start tv))
                :y (nn->height nn)
                :width (rescale-tv-time (tv/duration tv))
                :height height-per-stave}]))))

(defn bracket [from to height]
  (let [attrs {:stroke :black}]
    [:g
     [:line 
      (merge 
       attrs
       {:x1 from
        :y1 0
        :x2 to
        :y2 0})]
     [:line 
      (merge
       attrs
       {:x1 from
        :y1 0
        :x2 from
        :y2 height})]
     [:line
      (merge
       attrs
       {:x1 to
        :y1 0
        :x2 to
        :y2 height})]]))

(defn translate [x y & children]
  (into [:g {:transform (str "translate(" x " " y ")")}] children))

(defn vizualize-era [era]
  (let [path-value-map (core/era->path-value-map era)
        width 500
        per-rung 10
        rung-padding 5
        rung-total (+ rung-padding per-rung)
        piano-roll-height 300
        metas (into {} (filter (fn [[path tv]] (keyword? (tv/value tv))) path-value-map)) ;; todo -> rewrite with specter
        max-count (apply max (map count (keys metas)))
        last-end (last (sort (map (comp r/realize tv/end) (vals path-value-map))))
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        margin-height (* rung-total (inc max-count))
        total-height (+ margin-height piano-roll-height)]
    [:svg
     {:width width
      :height total-height}
     (for [[path tv] metas]
       (translate 0 (* rung-total (count path))
                  [:text {:dominant-baseline :hanging
                          :x (rescale-tv-time (tv/start tv))
                          :font-family :helvetica
                          :font-size 8
                          :y 0}
                   (str (name (tv/value tv)) (print-str path))]
                  (bracket (rescale-tv-time (tv/start tv))
                           (rescale-tv-time (tv/end tv))
                           per-rung)))
     (translate 0 margin-height
                (piano-roll width piano-roll-height path-value-map))]))
  
  (defn visualize-voices [era]
    (let [width 500
          height 100]
      ;; a potential 'rule' - if a voice is too long for its current level of parallelism, it inserts itself below as a new voice. 
      ;; The entire 'stack' gets pushed down, so to speak. 
      ;; voices are never merged nor split - they're calculated in advance 
      ;; parallel composition makes a new voice, and series composition makes a new voice when the bounds of the child are greater than the bounds it's alloted by porportion. (shift, scale, or chain)
      ;; the wing can only effect things in its current voice 
      ;; let's write the visualiser and associated algorithm for this. 

      ;; core/era->voice-seq
      [:svg {:width width
             :height height}]))
  
  (clerk/html 
   (vizualize-era 
    [:chain [:chain [1 2 3] [2 3 4]] [1 2 3]]))

  (comment
    
    (core/era->path-value-map (into [:chain] [1]))
    
    (vizualize-era 
     (into [:chain] [#{1} #{2 3}]))
    

    ;; tonality and qualia - tonality is a more specific version of 'qualia'
    
    [1 
     2 
     3 

     (sym 
      [[:sym/now-at :cello]
       [:sym/era-get [1 2 3]]
       [:sym/slice :cello] ;; returns exactly what's happening during the time this route is active
       ]
      ;; constructs and immediatley returns the dependencies. 
      ;; in order to avoid evaluating circular dependencies, does not immediatley calculate values.
      (fn [[a b]]
        (= a b )
        ))
     
     [(stream [:sym/era-get [1 2 3]])
      ;; or
      (stream (get-input :keyboard-manual))
      ;; or
      (stream [1 2 3])

      ;; the takeaway: many things can be cast to streams.
      ;; streams have children that are functions, which transform in real time the values appearing in the streams. 
      ;; streams abstract over 'start' and 'end' events in order to provide a similar interface to eras. 
      
      ;; 
      
      (sym
       [[:sym/now-at :cello]
        [:sym/era-get [1 2 3]]
        [:sym/slice :cello] ;; returns exactly what's happening during the time this route is active
        ]
            ;; constructs and immediatley returns the dependencies. 
            ;; in order to avoid evaluating circular dependencies, does not immediatley calculate values.
       (fn [[a b]]
         (fn [e]
           #{:many :notes} ;; note off is abstracted over  
           )))
      
      (fn [e] 
        #{:many :notes} ;; note off is abstracted over  
        )]
     
     ;; i see how values coming in could be used to set state, 
     ;; which means that more 'static' subscriptions could depend on them
     
     ;; what I don't see is how incoming values could be used more like an instrument with effects applied.
     ;; it seems that we need an additional class of 'thing', a live input channel
     
     ;; is a live 'stream' sufficiently different from a data subscription?
     ;; data sub - known time/events, potentially unknown / stateful data
     ;; live stream - unknown time/events, known ranges where subs are applicable.
     
     ;; incoming api - note name, all currently active notes, any active subs
     
     

     5] 

    (io/render-svg
     (vizualize-era
      [:chain
       [1 2 3 [:graft [:heap 4 5] 5] :> 6 1]
       [1 2 3 [:graft [:heap [4 5] 1] 5] 6 :>]
       [1 2 3 [:graft [:heap 4 5] 5] 6 :>]])
     "scratch/svgs/architecture.svg")
    
    (vizualize-era
     [:chain
      [[1 2 3 [:graft [:heap 4 5] 5] :> 6 1]]
      [1 2 3 [:graft [:heap 4 5] 5] 6 :>]
      [1 2 3 [:graft [:heap 4 5] 5] 6 :>]])

    (core/era->path-value-map
     [:heap
      [:chain [1 3] [2] 3 4]
      [:chain [0 0 0] [0 0 0]]])

    (get #{1 2 3} 2) 

    

    )