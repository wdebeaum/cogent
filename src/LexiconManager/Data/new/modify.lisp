;;;;
;;;; W::modify
;;;;

(define-words :pos W::v :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::modify
   (wordfeats (W::morph (:forms (-vb) :nom w::modification)))
   (SENSES
    (;(LF-PARENT ONT::modify)
     (LF-PARENT ONT::change)
     (example "modify the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    )
   )
))

