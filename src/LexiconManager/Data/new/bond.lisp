;;;;
;;;; W::bond
;;;;

(define-words :pos W::V :TEMPL AGENT-AFFECTED-XP-NP-TEMPL
 :words (
  (W::bond
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::attach)
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-TEMPL (xp (% w::pp (w::ptype w::to)))) ; like bind
     )
    #||((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("shake-22.3-2-1"))
     (LF-PARENT ONT::attach)
     (TEMPL agent-affected-xp-templ (xp (% w::pp (w::ptype w::to)))) ; like attach
     )||#
    )
   )
))

