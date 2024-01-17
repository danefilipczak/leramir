(ns leramir.notebooks.OSMD
  {:nextjournal.clerk/visibility {:code :fold}
   :nextjournal.clerk/toc true}
  (:require [nextjournal.clerk :as clerk] 
            [rational.core :as r]
            [leramir.era :as l]))

(def OSMD-viewer
  {:transform-fn clerk/mark-presented
   :render-fn '(fn [value]
                 (when value
                   [nextjournal.clerk.render/with-d3-require {:package ["opensheetmusicdisplay@1.8.3/build/opensheetmusicdisplay.min.js"]}
                    (fn [osmd]
                      [:div
                       {:ref (fn [el]
                               (when el
                                 (let [canvas (new (.-OpenSheetMusicDisplay osmd) el)]
                                   (.then 
                                    (.load canvas value)
                                    (fn [] (.render canvas))))))}])]))})


#_(def musicxmlWithTab (slurp "src/sfsymus/leramir/notebooks/musicxmlexamples/with-tab.xml"))

#_(clerk/with-viewer OSMD-viewer
  musicxmlWithTab)

(comment
  
  (clerk/serve! {:browse true})
  )