;;;;
;;;; W::prevent
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::prevent
   (wordfeats (W::morph (:forms (-vb) :nom w::prevention)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("forbid-67"))
     (LF-PARENT ONT::prevent)
     (example "prevent the sale")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 					; like block
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::prevent)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It prevents him from doing something")
     )
    )
   )
))

