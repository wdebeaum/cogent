;;;;
;;;; W::inhibit
;;;;

(define-words :pos W::v 
 :words (
  (W::inhibit
   (wordfeats (W::morph (:forms (-vb) :past w::inhibited :ing w::inhibiting :nom w::inhibition :agentnom w::inhibitor)))
   (SENSES
    (
     (LF-PARENT ONT::inhibit-effect)
     (example "it inhibited the sale")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 				
     )
    (
     (LF-PARENT ONT::inhibit-effect)
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing) (w::ptype w::from))))
     (example "It inhibited him from doing something")
     )
    )
   )
))
