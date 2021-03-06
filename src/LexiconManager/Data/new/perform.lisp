;;;;
;;;; W::perform
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
 (W::perform
   (wordfeats (W::morph (:forms (-vb) :nom w::performance)))
   (SENSES
    ((meta-data :origin calo :entry-date 20031230 :change-date nil :comments html-purchasing-corpus)
     (LF-PARENT ONT::EXECUTE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-neutral-xp-templ)
     (example "perform the activity/task") 
     )
    ((LF-PARENT ONT::act-behave)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-templ)
     (example "he performed for us ") 
     )
    )
   
   )
))
