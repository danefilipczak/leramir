(ns symus.laurelmir.viewer
  (:require [dali.syntax :as dali]
            [dali.io :as io]
            [symus.laurelmir :as l]
            [symus.laurelmir.types.timed-value :as tv]
            [symus.laurelmir.protocols.pitch :as pitch]
            [symus.laurelmir.rational :as r]
            [symus.laurelmir.utils.math :as utils.math]))

(defn spy [x]
  (println x)
  x)

(defn maybe-discreet [x]
  (when (satisfies? pitch/IPitch x)
    (pitch/discreet x)))

(defn piano-roll [width height p->tv]
  (let [pitches (sort (keep (comp maybe-discreet tv/value) (vals p->tv)))
        min-pitch (first pitches)
        max-pitch (last pitches)
        stave (reverse (range min-pitch (inc max-pitch))) 
        height-per-stave (float (/ height (count stave)))
        last-end (last (sort (map (comp r/realize tv/end) (vals p->tv)))) 
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        nn->i (zipmap stave (range))
        nn->height (fn [nn]
                     (assert (int? nn))
                     (* (nn->i nn) height-per-stave))
        ] 
    (concat (for [[i nn] (partition 2 (interleave (range) stave))]
              [:rect
               {:stroke :gainsboro :stroke-width 0.5 :fill (if (contains? 
                                                                #{0 2 4 5 7 9 11} 
                                                                (int (mod nn 12))) 
                                                             :white
                                                             :whitesmoke)}
               [0 (nn->height nn)] [width height-per-stave]])
            (for [tv (vals p->tv)
                  :let [nn (maybe-discreet (tv/value tv))]
                  :when nn]
              [:rect
               {:stroke :gainsboro :stroke-width 0.5 :fill :black}
               [(rescale-tv-time (tv/start tv)) (nn->height nn)] 
               [(rescale-tv-time (tv/duration tv)) height-per-stave]]))))

(defn bracket [from to height]
  (let [attrs {:stroke :black}]
    [:g
     [:line 
      attrs 
      [from 0] [to 0]]
     [:line 
      attrs 
      [from 0] [from height]]
     [:line
      attrs
      [to 0] [to height]]]))

(defn vizualize-era [era]
  (let [p->tv (l/->path->timed-value era)
        width 500
        per-rung 10
        rung-padding 5
        rung-total (+ rung-padding per-rung)
        piano-roll-height 300
        metas (into {} (filter (fn [[path tv]] (keyword? (tv/value tv))) p->tv)) ;; todo -> rewrite with specter
        max-count (apply max (map count (keys metas)))
        last-end (last (sort (map (comp r/realize tv/end) (vals p->tv)))) 
        rescale-tv-time (fn [tv-time] (float (utils.math/map-range (r/realize tv-time) 0 last-end 0 width)))
        margin-height (* rung-total (inc max-count))
        total-height (+ margin-height piano-roll-height)
        ]
    [:dali/page
     {:width width
      :height total-height} 
     (for [[path tv] metas]
       [:g {:transform [:translate [0 (* rung-total (count path))]]}
        [:text {:dominant-baseline :hanging 
                :x (rescale-tv-time (tv/start tv)) 
                :font-family :helvetica
                :font-size 8
                :y 0} 
         (name (tv/value tv))]
        (bracket (rescale-tv-time (tv/start tv)) 
                 (rescale-tv-time (tv/end tv)) 
                 per-rung)])
     [:g {:transform [:translate [0 margin-height]]}
      (piano-roll width piano-roll-height p->tv)]]))

  (comment
    [1 
     2 
     3 

     (plug 
      [[:plugs/now-at :cello]
       [:plugs/get-path [1 2 3]]] 
       (fn [[a b]]))
     
     (now-at :cello)

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

    (l/->path->timed-value
     [:heap
      [:chain [1 3] [2] 3 4]
      [:chain [0 0 0] [0 0 0]]])

    (get #{1 2 3} 2)

    ( l/->path->timed-value )

    (number? 1/3) 
    )