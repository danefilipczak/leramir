(ns sfsymus.leramir.ideas 
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.alpha :as s]
            [sfsymus.leramir :as l]))

(defprotocol IDepend
  (dependency-path [self]))

(defrecord NowIn [path]
  IDepend
  (dependency-path [self] (:path self)))

#_(defn now-in [path]
  (reify clojure.lang.IDeref
    (deref [_]
      (swap! counter inc)
      @counter)))

#_(l/->path-value-map
 [:heap
  [1 2 3 [#{2 3 4}]]
  @(now-in [0])
  @(now-in [1])
  @(now-in [2])])

#_(def my-counter (custom-counter))
#_@my-counter

  ; Output: 1

(def dependencies 
  {:a #{:b}
   :b #{:c}
   :c #{:x}})

(comment
  (l/era->path-value-map [1 [2 [3 4]]]) 
  
  (def dependencies
    {[] #{[0] [1]}
     [0] #{}
     [1] #{[1 0] [1 1]}
     [1 0] #{}
     [1 1] #{[1 1 0] [1 1 1]}
     [1 1 0] #{}
     [1 1 1] #{}
     })
  

  
  ;; every parent 'depends' on its child for its content. 
  ;; in this way, 

  ;; vents: spaces, grids, and voices
  (def x (quote (let [a 1 b 2] (+ a b))))
  (eval x)
  (eval (read-string (pr-str (quote (let [a 1 b 2] (+ a b))))))

  ;;this voice is being overwritten [[here]]


  [:stack 
   [:chain [1 2 3 4] 
    [:cello 5 6 7 8] ;; is overwritten by below in the stack
    [:cello 5 6 7 8]] ;; is not overwritten 
   [:chain [1 2 3 4] [:cello 5 6 7 8] [5 6 7 8]]]
  
  (detect-circular-dependency? dependencies)


  )

(comment
  

  (
   )



  (def modifiers #{:shift ;; modify by a rational value 
                   :offset ;; modify by a numeric value
                   }) 

  (spec/def ::props (spec/keys
                     :req [::now]
                     :opt [::dependancies]
                     :opt-un [::voice]))

  {:cello :flute
   :flute :bass
   :bass :cello}
  ;; disallowed -> dependency cycle

  (defn now [props]
    (:now props))

  [:era [1 2 3 (fn [props]
                  (grid-get-time :my-grid (now props)))]]


  ;; the whole point of the markup language is that it's static, not moving... 
  ;; once we introduce signals, we're in the realm of FRP, which is great, but at that point we probably need a macro

  )