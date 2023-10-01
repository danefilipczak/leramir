(ns leramir.notebooks.meander-rewriting
  (:require [meander.strategy.epsilon :as r]
            [meander.epsilon :as m]))


((r/rewrite :x :y) :x)

;; the data we pass in is called a reducible expression, or redex

(def any (r/rewrite 
          ?x [:match ?x]
          ))

(any 1)