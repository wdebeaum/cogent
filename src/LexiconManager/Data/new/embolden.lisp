;;;;
;;;; W::embolden
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::embolden
   (wordfeats (W::morph (:forms (-vb) :past W::emboldened :ing W::emboldening)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("embolden%2:37:00"))
     (LF-PARENT ONT::provoke)
     (example "the speech emboldened the masses")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) ; like annoy,bother,concern,hurt
     )
    )
   )
))

