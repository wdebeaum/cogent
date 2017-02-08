;;  Basic Conversational acts and convertional responses to various types of queries

(in-package :dagent)

;;(defvar *state-definitions* nil)


(add-segment 'talk-later
	     (segment
	       :trigger '((ONT::F ?!x ONT::TALK)
			  (ONT::F ?!y (:* ONT::EVENT-TIME-REL W::LATER))
			  -talk-later1>
			  (next))  ; was (true); changed to (nop); now changed to (next)
	       :start-state 'segmentend
	      ))

(add-segment 'ok
	     (segment
	       :trigger '((ONT::SPEECHACT ?!sa (? a ONT::ANSWER ONT::EVALUATE ONT::SA_EVALUATE ONT::FRAGMENT ONT::ACCEPT))
			  (ONT::F ?!x (:* ONT::GOOD (? w W::OKAY w::good)))
			  (ont::eval (find-attr :result nil ; to avoid competing with -user-response1>
						:feature PROPOSAL-ON-TABLE))
			  -ok1>
			  (next))
	       :start-state 'segmentend
	      ))

(add-segment 'cool
	     (segment
	       :trigger '((ONT::F ?!x (:* ONT::ACCEPTABILITY-VAL W::COOL))
			  -cool1>
			  (next))
	       :start-state 'segmentend
	      ))

; thanks, bye
(add-segment 'thanks
	     (segment
	       :trigger '((ONT::SPEECHACT ?!sa (? a ONT::SA_ACK ONT::SA_THANK ONT::SA_WELCOME ONT::CLOSE))
			  -thanks1>
			  (next))
	       :start-state 'segmentend
	      )
	     )

; good night, goodbye
(add-segment 'thanks
	     (segment
	       :trigger '((ONT::SPEECHACT ?!sa ONT::GREET :CONTENT (ONT::GOODBYE :CONTENT ?!c))
			  -goodbye1>
			  (next))
	       :start-state 'segmentend
	      )
	     )

; ok good night
(add-segment 'thanks
	     (segment
	       :trigger '((?spec ?!x (:* ONT::CONTENT W::GOOD-NIGHT))
			  -goodbye2>
			  (next))
	       :start-state 'segmentend
	      )
	     )

; ok bye
(add-segment 'thanks
	     (segment
	       :trigger '((?spec ?!x (:* ?!y W::BYE))  ; bye gets parsed as ONT::ACTING
			  -goodbye3>
			  (next))
	       :start-state 'segmentend
	      )
	     )

(add-state 'checkchannel 
 (state :action '(generate :content (ONT::GREET)) ;'(SAY :content "hello" "i'm here")
  :transitions (list
		(transition
		 :description "Greeting"
		 :pattern '((ONT::SPEECHACT ?!sa ONT::GREET :who ?!sp) 
			    -checkchannel1> 
			    ;(SAY-ONE-OF :content ("hello" "hi" "hey" "I'm here."))
			    (generate :content (ONT::GREET)))
		 :trigger t
		 :destination 'segmentend)
		(transition
		 :description "Hello?"
		 :pattern '((ONT::SPEECHACT ?!sa ONT::PROMPT :who ?!sp) 
			    -checkchannel2> 
			    ;(SAY-ONE-OF :content ("I'm here." "I got your message!"))
			    (generate :content (ONT::GREET)))
		 :trigger t
		 :destination 'segmentend)
		(transition
		 :description "I'm up"
		 :pattern '(((? spec ONT::F ONT::BARE) ?!x ONT::HAVE-PROPERTY)
			    (ONT::F ?!y (:* ONT::SCALE-RELATION W::UP))
			    -checkchannel3> 
			    ;(SAY-ONE-OF :content ("good morning" "hi!"))
			    (generate :content (ONT::GREET)))
		 :trigger t
		 :destination 'segmentend)
		)
  ))
		


(add-state 'ack (state :action nil
		       :implicit-confirm t
		       :transitions
		       (list
			(transition
			 :description "acknowledge"
			 :pattern '((ONT::F ?!Y (:* ONT::GOOD W::OKAY))
				    -ack1>
				    (next))
			 :destination 'segmentend)
			)))
		  

;;;;;;;;;;
(add-preprocessing 'how-often
 (list 
  (rule
   :description "Not at all"
   :pattern '((ONT::F ?!x (:* ONT::DEGREE-MODIFIER W::NOT-AT-ALL))
	      -how-often1>
	      (HOWOFTEN :value 0))
   )
  (rule
   :description "Not at all"
   :pattern '((ONT::F ?!x (:* ONT::AVAILABILITY-VAL W::AT-ALL))
	      (ONT::F ?!x2 ONT::NEG)
	      -how-often1b>
	      (HOWOFTEN :value 0))
   )
  (rule
   :description "none, two"
   :pattern '((?!spec  ?!x ?type :size ?!w)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!w ?n))
	      -how-often2>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "five"
   :pattern '((ONT::INDEF-SET  ?!x ?type :number ?!w)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!w ?n))
	      -how-often3>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "once"
   :pattern '((ONT::F ?!x (:* ONT::EVENT-TIME-REL w::once))
	      -how-often4>
	      (HOWOFTEN :value 1))
   )
  (rule
   :description "once, twice"
   :pattern '((ONT::F ?!f (:* ONT::REPETITION ?!w))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!w ?n))
	      -how-often5>
	      (HOWOFTEN :value ?n))
   )
  (rule   ; this is subsumed by how-often2
   :description "A few times, several times"
   :pattern '((ONT::INDEF-SET ?!x  (:* ONT::TIME-POINT ?!c) :SIZE ?!size)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!size ?n))
	      -how-often6>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "A couple of times"
   :pattern '((ONT::INDEF-SET ?!x  (:* ONT::TIME-POINT ?!c))
	      (ONT::F ?!x1  (:* ONT::MODIFIER W::A-COUPLE-OF))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::A-COUPLE-OF ?n))
	      -how-often7>
	      (HOWOFTEN :value ?n))
   )
  (rule
   :description "three times"
   :pattern '((?!spec ?!x  (? x1 ONT::TIME-INTERVAL ONT::TIME-POINT) :SIZE ?!size)
	      (ONT::A ?!size ONT::NUMBER :value ?!num)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!num ?n))
	      -how-often8>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "most of the time"
   :pattern '((?!spec ?!size (? x ONT::TIME-INTERVAL ONT::TIME-POINT) :quan ?!q :NEGATION -)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!q ?n))
	      -how-often9>
	      (HOWOFTEN :value ?n))
   )
 
  (rule
   :description "occasionally, frequently, often, never"
   :pattern '((ONT::F ?!x  (:* ONT::FREQUENCY ?!freq) :FORCE ONT::TRUE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!freq ?n))
	      -how-often10>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "not often"  ; "not occasionally": if they say that, will need another value
   :pattern '((ONT::F ?!x  (:* ONT::FREQUENCY ?!freq) :FORCE ONT::FALSE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!freq ?n))
	      -how-often10b>
	      (HOWOFTEN :value 1))
   )
  
  (rule
   :description "not very (often)"
   :pattern '((ONT::F ?!x  (:* ONT::DEGREE-MODIFIER-HIGH W::VERY))
	      (ONT::F ?!y ONT::NEG)
	      -how-often10c>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "alot, plenty, a little"
   :pattern '((ONT::A ?!x ?!type :QUAN ?!q)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ?!q ?n))
	      -how-often11>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :FORCE ONT::TRUE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::a-lot ?n))
	      -how-often12>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "not a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :FORCE ONT::FALSE)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::not-a-lot ?n))
	      -how-often12b>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION -)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::a-lot ?n))
	      -how-often12c>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "not a lot"
   :pattern '((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION +)
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::not-a-lot ?n))
	      -how-often12d>
	      (HOWOFTEN :value ?n))
   )

  (rule
   :description "a bit"
   :pattern '((Ont::A ?!x ONT::QUANTITY :unit (:* ONT::MEMORY-UNIT ONT::BIT))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER ont::a-bit ?n))
	      -how-often13>
	      (HOWOFTEN :value ?n))
   )
  
  (rule
   :description "one time"
   :pattern '((?!spec ?!x  (? x1 ONT::TIME-INTERVAL ONT::TIME-POINT))
	      (?!spec2 ?!y (:* ONT::REFERENTIAL-SEM W::ONE))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::ONE ?n))
	      -how-often14>
	      (HOWOFTEN :value ?n))
   )

  (rule 
   :description "one"
    :pattern '((?!spec2 ?!y (:* ONT::REFERENTIAL-SEM W::ONE))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::ONE ?n))
	      -how-often14a>
	       (HOWOFTEN :value ?n)))

  (rule
   :description "zero"
   :pattern '((?!spec ?!x  (:* ONT::NUMBER-RELATED-PROPERTY-VAL W::ZERO))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::ZERO ?n))
	      -how-often15>
	      (HOWOFTEN :value ?n))
   )

  ; ??? this can't work ???
  #|
  (rule
   :description "a couple"
   :pattern '((ONT::A ?!x  (:* ONT::QUANTITY -) :UNIT (:* ONT::QUANTITY ONT::COUPLE))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::A-COUPLE ?n))
	      -how-often16>
	      (HOWOFTEN :value ?n))
   )
  |#

  (rule
   :description "a couple"
   :pattern '((ONT::A ?!x  ONT::QUANTITY :UNIT (:* ONT::QUANTITY ONT::COUPLE))
	      (ONT::EVAL (FREQUENCY-TO-NUMBER W::A-COUPLE ?n))
	      -how-often16>
	      (HOWOFTEN :value ?n))
   )
  
  (rule
   :description "not much"
   :pattern '((ONT::F ?!x (:* ONT::DEGREE-MODIFIER-HIGH W::MUCH))
	      (ONT::F ?!y ONT::NEG)
	      -how-often17>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "not very much"
   :pattern '((ONT::F ?!x (:* ONT::PREDICATE W::VERY-MUCH))
	      (ONT::F ?!y (:* ONT::NEG W::NOT))
	      -how-often18>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "very much"
   :pattern '((ONT::F ?!x (:* ONT::PREDICATE W::VERY-MUCH) :FORCE ONT::TRUE)
	      -how-often19>
	      (HOWOFTEN :value 3))
   )

  (rule
   :description "not many times"
   :pattern '((?!spec ?!size (? x ONT::TIME-INTERVAL ONT::TIME-POINT) :quan ONT::MANY :NEGATION +)
	      -how-often20>
	      (HOWOFTEN :value 1))
   )

  (rule
   :description "multiple times"
   :pattern '((?!spec ?!size (? x ONT::TIME-INTERVAL ONT::TIME-POINT))
	      (ONT::F ?!x2 (:* ONT::FREQUENCY-VAL W::MULTIPLE))
	      -how-often21>
	      (HOWOFTEN :value 3))
   )

  ))


   
;;;;;;;;;;
(add-preprocessing 'yes-no
		   (list
		    (rule
		     :description "no"
		     :pattern '((ONT::SPEECHACT ?!s ONT::ANSWER :what ONT::NEG) 
				-yes-no-no1> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "not yet"
		     :pattern '((ONT::F ?!s ONT::CONTINUATION) 
				(ONT::F ?!x ONT::NEG)
				-yes-no-no2> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "I didn't"
		     :pattern '((ONT::F ?!s (:* ONT::ELLIPSIS) :force ONT::FALSE) 
				-yes-no-no3> 
				(ANSWER :value NO)))
		    (rule
		     :description "not now/today"
		     :pattern '((ONT::F ?!s (:* ONT::EVENT-TIME-REL (? w W::NOW W::TODAY))) 
				(ONT::F ?!x ONT::NEG)
				-yes-no-no4> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "not really"
		     :pattern '((ONT::F ?!s (:* ONT::DEGREE-OF-BELIEF W::REALLY) :FORCE ONT::FALSE) 
				-yes-no-no5> 
				(ANSWER :value NO))
		     )
		    (rule
		     :description "yes"
		     :pattern '((ONT::SPEECHACT ?!s ONT::ANSWER :what ONT::POS) 
				-yes-no-yes1> 
				(ANSWER :value YES)))
		     (rule
		     :description "I did"
		     :pattern '((ONT::F ?!s (:* ONT::ELLIPSIS) :force ONT::TRUE) 
				-yes-no-yes2> 
				(ANSWER :value YES)))
		    
		    ))



(add-preprocessing 'yes-no-others
		   (list
		    ; should really record current time minus 10 minutes
		    ; now only records current time
		    (rule 
		     :description "10 minutes ago; a while ago"
		     :pattern '((?!sp ?!x (:* ONT::EVENT-TIME-REL W::AGO)) 
				-yoo1> 
				(ANSWER :value YES))
		     )

		    (rule 
		     :description "I forgot"
		     :pattern '(((? spec ONT::F ONT::BARE) ?!sa (:* ONT::FORGET W::FORGET))
				-yoo6> 
				(ANSWER :value NO))
		     )

		    (rule 
		     :description "ok"
		     :pattern '(((? spec ONT::F ONT::BARE) ?!sa (:* ONT::GOOD W::OKAY))
				-yoo7> 
				(ANSWER :value NO))
		     )))
    
;;;;;;;;;;      
;;  answering how good/bad questions
(add-preprocessing 'qual-good-bad
 (list 
  (rule
   :description "not bad"
   :pattern '((ONT::F ?!x (:* ONT::BAD ?!w));; :mod (?!z))
	      (ONT::F ?!Z (:* ONT::NEG ?!neg))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::BAD ?!w ?!neg  ?score))
	      -qual-good-bad3> 
	      (GOODBAD :value ?score :qual (?!neg ?!w))))
  (rule
   :description "not good"
   :pattern '((ONT::F ?!x (:* ONT::GOOD ?!w));; :mod (?!z))
	      (ONT::F ?!Z (:* ONT::NEG ?!neg))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::GOOD ?!w ?!neg  ?score))
	      -qual-good-bad4> 
	      (GOODBAD :value ?score :qual (?!neg ?!w))))

  (rule
   :description "bad"
   :pattern '((ont::F ?!x (:* ONT::BAD ?!w))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::BAD ?!w nil ?score))
	      -qual-good-bad1>
	      (GOODBAD :value ?score :qual ?!w)))

  (rule
   :description "good"
   :pattern '((ONT::F ?!x (:* ONT::GOOD ?!w))
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::GOOD ?!w nil ?score))
	      -qual-good-bad2> 
	      (GOODBAD :value ?score :qual ?!w)))	 
   
  (rule
   :description "I'm good/bad, good"
   :pattern '((ONT::F ?!x (:* ont::Acceptability-val ?!w)) ;; :MODS (?!m))
	      (ONT::F ?!m (:* ONT::DEGREE-MODIFIER ?!mod))
	      (ONT::EVAL (GOOD-BAD-SCORE nil ?!w ?!mod ?score))
	      -qual-good-bad6>
	      (GOODBAD :value ?score :qual (?!mod ?!w))))
  
  (rule
   :description "I'm good/bads, good"
   :pattern '((ONT::F ?!x (:* ont::Acceptability-val ?!w))
	      (ONT::EVAL (GOOD-BAD-SCORE nil ?!w nil ?score))
	      -qual-good-bad5>
	      (GOODBAD :value ?score :qual ?!w)))

  (rule
   :description "good"
   :pattern '((ONT::QUANTIFIER ?!x (:* ONT::PROBLEM W::PROBLEM) :QUAN ONT::NONE)
	      (ONT::EVAL (GOOD-BAD-SCORE ONT::GOOD ?w nil ?score))
	      -qual-good-bad7> 
	      (GOODBAD :value ?score :qual ?w)))	 

    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  classifying how much of a quality (e.g., how limited, how happy) on a 0 to 4 scale
(defvar *howmuchqual*)

(setq *howmuchqual*
  ' (
     (0 ; not at all
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER-VERYLOW)))
      ((ONT::INDEF-SET ?!x ?!t :SIZE ONT::NONE))

      ((ONT::F ?!x (:* ONT::AVAILABILITY-VAL W::AT-ALL))
       (ONT::F ?!x2 ONT::NEG))

      )
     (1  ;; slightly
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::A-LITTLE W::A-BIT))))
      ((?spec ?!x ?!t :quan ont::A-LITTLE))
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::MUCH W::VERY W::A-LOT))) ;;:MODS (?!y))
       (ONT::F ?!y (:* ONT::NEG W::NOT)))
      ((ONT::F ?!x ONT::LITTLE))
      ((ONT::F ?!x ONT::LITTLE)
       (ONT::F ?!y ONT::DEGREE-MODIFIER :OF ?!x))
      ((Ont::A ?!x ONT::QUANTITY :unit (:* ONT::MEMORY-UNIT ONT::BIT)))  ;;  handle misparsing of "a bit"
      ((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION +))  ;; not a lot
      )

     (2  ;; moderately
      ((ONT::F ?!x (:* ONT::SEVERITY-VAL W::MODERATELY)))
      ((ONT::INDEF-SET ?!x ?!t :SIZE ONT::SOME))
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::SOMEWHAT))))
      )

     (3  ;; very
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::VERY W::A-LOT W::WELL))))
      ((ONT::INDEF-SET ?!x ?!t :SIZE ONT::PLENTY))
      ((ONT::A ?!x (:* ONT::AREA-DEF-BY-USE W::LOT) :NEGATION -))
      
      )

     (4 ;; totally
      ;; all the time (need to fix parser)
      ((ONT::F ?!x (:* ONT::PART-WHOLE-VAL W::TOTALLY)))
      ((ONT::F ?!x (:* ONT::DEGREE-MODIFIER (? w W::A-GREAT-DEAL W::COMPLETELY))))
       
      )
     )
    )
(define-rules 'howmuchqual *howmuchqual* 'howmuchqual :value)

#|

(defun words (var)
  (result var *words*))

(defun current-time (var)
  (result var (get-time-of-day)))

(defun time-before (time)
  (if (t-before (get-time-of-day) time)
      im::*success*))
   
(defun result (n x)
  (im::match-with-subtyping n x))

(defun find-attr (var name)	
  (result var (get-attr *current-user* name)))

||#
