;;;;
;;;; W::throw
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::throw
   (wordfeats (W::morph (:forms (-vb) :past W::threw :pastpart W::thrown :nom w::throw)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("throw%2:31:00" "throw%2:37:00"))
     (LF-PARENT ONT::evoke-confusion)
     (example "the detour threw them until they looked at the map")
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::propel)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "he threw the ball")
     (meta-data :origin step :entry-date 20080721 :change-date nil :comments nil)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  ((W::throw (W::up))
   (wordfeats (W::morph (:forms (-vb) :past W::threw :pastpart W::thrown)))
   (SENSES
    ((LF-PARENT ONT::visual-display)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "throw up the map")
     )
    ((EXAMPLE "The patient threw up")
     (LF-PARENT ONT::vomit)
     (TEMPL affected-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  ((W::throw (W::off))
   (wordfeats (W::morph (:forms (-vb) :past W::threw :pastpart W::thrown)))
   (SENSES
    ((EXAMPLE "My schedule is all thrown off ")
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL) 
     )
    )
   )
))

