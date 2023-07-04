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

(defn piano-roll [flight]
  (let [width 500
        height 500
        p->tv (l/->path->timed-value flight)
        pitches (sort (keep (comp maybe-discreet tv/value) (vals p->tv)))
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
    [:dali/page
     (for [[i nn] (partition 2 (interleave (range) stave))]
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
        [(rescale-tv-time (tv/duration tv)) height-per-stave]])]))

(comment
  (l/->path->timed-value
   [:heap
    [:chain [1 3] [2] 3 4]
    [:chain [0 0 0] [0 0 0]]])

  (get #{1 2 3} 2)

  ( l/->path->timed-value )

  (number? 1/3) 

  (io/render-svg
   (piano-roll
    [:chain 
     [[1 2 3 [:graft [:heap 4 5] 5] :> 6 1]]
     [1 2 3 [:graft [:heap 4 5] 5] 6 :>]
     [1 2 3 [:graft [:heap 4 5] 5] 6 :>]])
   "scratch/svgs/architecture.svg")
  )