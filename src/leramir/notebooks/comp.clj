(ns leramir.notebooks.comp
  {:nextjournal.clerk/visibility {:code :fold}
   :nextjournal.clerk/toc true}
  (:require [nextjournal.clerk :as clerk]
            [leramir.viewer :as viewer]
            [clojure.math.combinatorics :as combo]
            [leramir.rational :as r]
            [leramir.era :as l]))

;; # the Signature
;; tonal materials are derived from D A N (neighbortone) E
;; rhythmic materials are derived from dane filipczak (long short short long)
(clerk/html
 (viewer/vizualize-era
  [:era {:color :grey} 
   2 [9 8] 4]))

(clerk/html
 (viewer/vizualize-era
  [2 9
   [:era {:color :green :scale (r/rational 2 1)} [8 [4 3]]]
   4]))

(clerk/html
 (viewer/vizualize-era
  [2 9
   [:era {:color :green :shift (r/rational -1 1)} [8 [4 3]]]
   4]))

(comment
  (clerk/serve! {:browse true}))

;; # Tonal materials

(def dane [0 7 :n 2])
(def opening #{2 9 5})
(def chords [opening 
             #{2 5 10} 
             #{7 2 10} 
             #{7 3 10}
             (into #{} (map inc opening))])

;; there is a corresponding progression that can be constructed off each index of this progression.
;; In each case, the starting chord ends up a semnitone higher. 
;; The fifth is the same as the first, completing the cycle. 

(def progressions 
  [
   [:l :r :l :p] 
;; m leads to m+1
;; M leads to M-1
   [:r :l :p :l] 
;; m leads to m-1
;; M leads to M+1
   [:l :p :l :r]
;; m leads to m+1
;; M leads to M-1
   [:p :l :r :l]
;; m leads to m-1
;; M leads to M+1
   [:l :r :l :p]]
  ;; m leads to m+1
;; M leads to M-1
  )

;; is this a universal property of RPL progressions? As far as I can tell, any triad of the same type encountered in an rpl progression will be in a mirror position to the same progression started on a minor chord. 
(take 5 (repeatedly #(rand-nth [:r :p :l])))

(clerk/html
 (viewer/vizualize-era
  (into [:chain] chords)))

(clerk/html
 (viewer/vizualize-era
  (into [:chain] [#{1 2 3} #{2 3 4}]))) 

(leramir/era->path-value-map chords)

;; ### manners of treatment of the progression
;; - one cycle of the progression with a bass drone throughout on the fifth of the final chord. 
(def treatment-1 ::one-cycle-bass-drone)
;; - three cycles of the progression with conjunct bass motion. Accent on the first chord of the 4th go around, then 4th or 5th based cadence that repeats at least once. 
;;   - this has a higher rate of difference, potentially more destabalizing. 
(def treatment-2 ::multi-cycle-conjunct-bass)
;; since there are two variations of each, a minor and a major, this leads to four progressions constrained to occur at least once in each piece. 

;; todo: find out the equivilent to this progression in four and five note chord space. 
;; this needs to start with a definition of what exactly counts as an object in these spaces
;; in RPL space, we're not including the maximally even entity (the augmented triad)

;; to learn to do this properly in 4 and 5 note space is going to require some fundamental learning in diatonic set theory. 
;; let's do that as an iteration after this composition - 
;; in the meantime, we will implement a hueristic. 
;; in the same way that we define RPL as transformations between the two most-even-but-not-symmetrical three note sets, 
;; we will define a new grammar of transformations between the three most-even-but-not-symmetrical four note chords, 
;; the dominant, m-7, and half diminished.
;; n - 1 entities in each set. After defining this grammar, it will be interesting to see if the structural properties of major/minal dualism are preserved amongst dual chords. 


(def pattern (->> chords
                  (partition 2 1)
                  (map
                   (fn [[a b]]
                     [(first (clojure.set/difference a b)) 
                      (first (clojure.set/difference b a))]))))

(clerk/html
 (viewer/vizualize-era
  pattern))

(defn parsons-code [x]
  (->>
   x
   flatten
   (partition 2 1)
   (map (fn [[a b]]
          (cond
            (= a b) :r
            (> a b) :d
            (< a b) :u)))))

(defn dual [parsons-code]
  (map (fn [x] (get {:u :d :d :u} x)) parsons-code))

;; # four permutations of a contour
(def contours 
  #{(parsons-code pattern)
    (dual (parsons-code pattern))
    
    (parsons-code (reverse (flatten pattern)))
    ;; is the dual of
    (reverse (parsons-code pattern))
    ;; which is not intuitive
    })
;; is each used in every movement?
;; when we disjoin the empty set from all the subsets of the 4 contours, we have 15
(rest (map set (combo/subsets (seq contours)))) 

;; applying a partitioning scheme that spreads out the cardinality of the contour sets among the 5 movements. 
(->> (rest (map set (combo/subsets (seq contours))))
     (partition 3)
     (apply interleave)
     (partition 3))


;; # Conceptual materials 

;; ## Basic setup
;; the number 5

(def n 5)

;; five elements

(def empty-elements (take n (repeat nil)))

;; performable in a moderate timespan - between 30 and 40 minutes. 
(def approximate-timespan (/ (+ 30 40) 2))

(defn with-timespan [e]
  (map
   (fn [x]
     (assoc x :approximate-timespan (/ approximate-timespan n)))
   e))

(def elements-with-timespan (with-timespan empty-elements))

;; ## folding in sets of five

;; ### the big 5 mass extinctions

(defn mya->bwa [mya]
  (let [billion-years-in-mya 1000 ;; One billion years equals 1,000 million years
        weeks-in-a-year 52]
    (float (/ (* mya weeks-in-a-year) billion-years-in-mya))))

(def mass-extinctions
  (->> [{:event "Ordovician-Silurian Extinction"
         :mya 443
         :periods ["Ordovician" "Silurian"]
         :description "This extinction event occurred during the transition from the Ordovician to the Silurian period. It is believed to have been caused by a combination of glaciation and rapid climate change, resulting in a drop in sea levels. It affected mostly marine life, including trilobites and brachiopods."}
        {:event "Late Devonian Extinction"
         :mya (/ (+ 359  375) 2)
         :periods ["Devonian" "Carboniferous"]
         :description "This extinction event happened during the Late Devonian period and had multiple phases. The causes are still debated but likely include a series of environmental changes, such as sea-level fluctuations and anoxic events (low oxygen conditions). It affected marine life, including many reef-building organisms."}
        {:event "Permian-Triassic Extinction"
         :mya 252
         :periods ["Permian" "Triassic"]
         :description "The Permian-Triassic extinction is the most severe mass extinction in Earth's history. It marked the boundary between the Permian and Triassic periods. The causes are complex and may include volcanic activity, climate change, and ocean acidification. It is estimated that about 96% of marine species and 70% of terrestrial vertebrate species went extinct during this event."}
        {:event "Triassic-Jurassic Extinction"
         :mya 201
         :periods ["Triassic" "Jurassic"]
         :description "This extinction event occurred during the transition from the Triassic to the Jurassic period. The exact causes are still uncertain, but possible factors include volcanic activity, climate change, and the breakup of the supercontinent Pangaea. The event affected various groups of marine and terrestrial organisms, including some early dinosaurs."}
        {:event "Cretaceous-Paleogene Extinction"
         :mya 66
         :periods ["Cretaceous" "Paleogene"]
         :description "This is one of the most famous extinction events as it marks the end of the Cretaceous period and the Mesozoic Era. It is believed to have been triggered by the impact of a large asteroid or comet, creating the Chicxulub crater in what is now Mexico. The resulting environmental disruptions, including wildfires, tsunamis, and long-term climate changes, led to the extinction of the non-avian dinosaurs and many other species."}]
       (map (fn [x] (assoc x :bwa (mya->bwa (:mya x)))))))

(defn with-extinctions [e]
  (->> (zipmap mass-extinctions e)
       (map (fn [[extinction element]]
              (merge element extinction)))))

;; ## 5 Stones

;; 1 Samuel 17:40:
;; > And he took his staff in his hand, and chose him five smooth stones out of the brook, and put them in a shepherd's bag which he had, even in a scrip; and his sling was in his hand: and he drew near to the Philistine.

(defn with-stones [e]
  (map
   (fn [x] (assoc x :contains #{"one smooth stone"}))
   e))

;; ## Pentateuch
(def pentateuch
  [{:book "genesis"
    :synecdoches [{:synecdoche "the garden" :notes []}
                  {:synecdoche "joseph's coat" :notes []}
                  {:synecdoche "jacob's ladder"
                   :notes ["a ladder to heaven that appeared in a dream"]}]}

   {:book "exodus"
    :synecdoches [{:synecdoche "the tabernacle" :notes []}
                  {:synecdoche "burning bush" :notes []}
                  {:synecdoche "The Parting of the Red Sea" :notes []}
                  {:synecdoche "the Decalogue" :notes []}]}

   {:book "leviticus"
    :synecdoches [{:synecdoche "Burnt Offerings" :notes []}
                  {:synecdoche "\"Holiness Code\" (Leviticus 17-26)" :notes []}
                  {:synecdoche "\"The Day of Atonement\"" :notes []}]}

   {:book "numbers"
    :synecdoches [{:synecdoche "forty years" :notes []}
                  {:synecdoche "the twelve spies" :notes []}
                  {:synecdoche "The Water from the Rock" :notes []}]
    :notes ["twelve spies should become thematic, as in: 'I sent the first spy to the river park'. Here 'river park' is a metonym for the promised land."
            "forty years should be divided by 12 to become 'one third of ten', and that should appear twelve times, once per each 'spy' section."]}

   {:book "deuteronomy"
    :synecdoches [{:synecdoche "\"The Shema\" (Deuteronomy 6:4-5)"
                   :notes ["Using the declaration \"Hear, O Israel: The Lord our God, the Lord is one\" as a synecdoche for the broader content of Moses' final speeches to the Israelites."]}
                  {:synecdoche "\"The Covenant\" (Deuteronomy 29:1)" :notes []}
                  {:synecdoche "Cities of Refuge" :notes []}
                  {:synecdoche "The Land Flowing with Milk and Honey" :notes []}]}])

(defn with-pentateuch [e]
  (->> (zipmap e pentateuch)
       (map (partial apply merge))))


;; # todo: the stages of education 
;; # todo: the 5 precepts
;; # todo: the 5 stages of meditation
;; # todo: the 5 energy transition[]

(def elements-with-concepts
  (-> elements-with-timespan
      with-extinctions
      with-stones
      with-pentateuch))

(nth elements-with-concepts 2)


;; # Structural Materials

;; ## inception - song within a song
;; this seems too specific to maintain over more than about half. Indeed, more than over two.

(def inceptions (map (fn [x] (when x {:contains #{x}})) (take n (interleave (repeat nil) (repeat ::inception)))))

(defn with-inception [e]
  (->> (zipmap e inceptions)
       (map (partial apply (partial merge-with clojure.set/union)))))

(def elements-with-structure
  (-> elements-with-concepts
      with-inception))


;; # Prosodic materials
;; the foot it restricted by 

(def s "stressed")
(def u "unstressed")

;; two syllable
(def spondee [s s])
(def iamb [u s])
(def trochee [s u])
(def pyrrhus [u u])
;; three syllable
(def tribrach [u u u])
(def anapest [u u s])
(def dactyl [s u u])
(def amphibrach [u s u])
(def bacchius [u s s])
(def cretic [s u s])
(def antibacchius [s s u])
(def molossus [s s s])


;; # Rhythmic Materials

contours

;; each of our pitch contours follows the shape x o x o x o o or x o o x o x o
(def rhythmic-cell-1 [dactyl trochee trochee])
(def rhythmic-cell-2 [trochee trochee dactyl])

;; # vocal materials 
;; nasal overtones / timbre, modeled as boolean
(def nasal? false)
;; with modulation, modeled as boolean
(def modulation? false)

;; # ornamentation materials
;; ornamentation is sub-tonal tonal material - something that does not show up in the basic structure of a line, but rather can be folded  into any conceivable note, or seem between two notes. 
;; ## the double backtrack
;; a discreet oscillation between the target note and the current note, twice
(def target 60)
(def current 62)
[current :sustain target current target current target :sustain]
;; ## portamento
;; continuous curve between source and target pitch
;; ## sample based restart
;; using a sampler to restart a phrase from the beginning along a rhythmic pattern. 
;; especially when using one of the rhythmic cells in diminution
[rhythmic-cell-1 rhythmic-cell-2]
;; variation: pitch displacement of one of the restarts


;; # Instrumentation
;; the six mainsies, plus digitally sequenced SWAM instruments with your breath blown into them
;; the swams are treated using timbre displacement, and that midi data is fed back into the visualizer

;; Visualization 
;; each of the mainsies gets a different pheme shape in a piano-style visualizer
;; swam instruments have continuous lines that map to their midi expression data. 
;; there is a 'now' thing 




