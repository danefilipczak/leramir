(ns leramir.core
  (:require [leramir.ast]))

(defn era->path-value-map [era] 
  (leramir.ast/era->path-value-map-via-ast era))