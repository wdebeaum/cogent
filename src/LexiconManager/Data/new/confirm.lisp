;;;;
;;;; W::confirm
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::confirm
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::CONFIRM)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "confirm that they arrived")
     (TEMPL AGENT-FORMAL-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::confirm)
     (example "he confirmed the information (with him)")
     (TEMPL AGENT-neutral-xp-TEMPL)
     )
    ((LF-PARENT ONT::confirm)
     (example "I confirmed it to be broken")
     (TEMPL AGENT-AFFECTED-FORMAL-CP-OBJCONTROL-TEMPL)
    )
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result confirmed that the gene activates the protein")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-FORMAL-XP-NP-1-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )

    ((LF-PARENT ONT::confirm)
     (example "we confirm in this paper that it works")
     (preference .98)
     (TEMPL AGENT-FORMAL-LOCATION-2-XP-TEMPL (xp (% w::cp (w::ctype w::s-finite))))
     )
    
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result confirmed the hypothesis")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL NEUTRAL-NEUTRAL1-XP-TEMPL)
     )

   ))
))

