(ns leramir.midi)


(comment
  ;; when writing to a midi file, we have ticks (in microseconds per quarternote), time sig, and delta time.
  ;; what if time sig is 1/4 and every era is one quarter note? 
  ;; then our output is simple - one era per quarter note
  

  
  ;; we should give the option to quantize to other time signatures if they wish, but keep 1/4 as the default. 
  ;; it drives home the notion that the era is the ultimate unit of time. 
  

  ;; in order to calculate the midi file, 
  ;; we first need a list of all events (note on and note off)
  ;; it would be helpful if there was an intermediary 'preprocessing' step wherein notes in the same output class have their note-offs reconciled
  ;; then, we sort the list by time.
  ;; we calculate the delta (as an integer of ticks)
  

  ;; 
  


  (def era [1 2 3 
            ;; (subs etc)
            ])
  
  ;; when paths do not start with a number, they are assumed to be internal to the sketch.
  ;; all paths should be prefaced by a non-integer uuid 
  

  (play (defscore [(subs )]))

  ;; should there be an explicit *metro* like object? 
  ;; in other words, should you be able to program via temporal recursion to a live running score?
  ;; I think that, maybe, you should
  
  (take 1 (repeat 3)) 

  (ditto [:heap 1 2 3])

  (play [:heap
         [:loop [:chain (repeat [1 2 3])]]
         [1 2 3]])

  (play 
   [:heap 
    [:loop [:chain (repeat [1 2 3])]]
    [1 2 3]])


  )