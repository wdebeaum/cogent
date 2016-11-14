;;  Basic Dialogue Agent State Management

(in-package :dagent)

;;(defvar *state-definitions* nil)

(add-state 'propose-cps-act
 (state :action nil ;;'(SAY-ONE-OF  :content ("What do you want to do?"))
	:transitions
	(list
	 #|
	 (transition
	 :description "Forget it.  Let's start over."
	 :pattern '((ONT::SPEECHACT ?!sa (? x ONT::PROPOSE ONT::REQUEST) :what ?!what)
	 (?spec ?!what (? t ONT::FORGET ONT::CANCEL ONT::RESTART))
	 (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
	 (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
	 -propose-restart>
	 (RECORD  CPS-HYPOTHESIS (PROPOSE :content ?!what :context ?akrl-context :active-goal ?goal))
	 (INVOKE-BA :msg (INTERPRET-SPEECH-ACT :content (PROPOSE :content ?!what :context ?akrl-context
	 :active-goal ?goal))))
	 :destination 'handle-csm-response
	 :trigger t)
	 |#

	 (transition
	  :description "proposal/request. eg: Let's build a model/I need to find a treatment for cancer"
	  :pattern '((ONT::SPEECHACT ?!sa (? x ONT::PROPOSE ONT::REQUEST) :what ?!what)
		     ;; ((? spec ONT::EVENT ONT::EPI) ?!what ?!t)
		     (?!spec ?!what ?!t)
		     (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
		     (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
		     -propose-goal>
		     (RECORD CPS-HYPOTHESIS (PROPOSE :content ?!what :context ?akrl-context :active-goal ?goal))
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (PROPOSE :content ?!what
							:context ?akrl-context
							:active-goal ?goal)))
		     )
	  :destination 'handle-csm-response
	  :trigger t)
	 
	 (transition
	  :description "ask-wh. eg: what drug should we use?"
	  :pattern '((ONT::SPEECHACT ?!sa (? s-act ONT::ASK-WHAT-IS) :what ?!what)
		     (?!spec ?!what ?!object-type)
		     (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))  
		     (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
		     -propose-goal-via-question>
		     (RECORD CPS-HYPOTHESIS (ONT::ASK-WHAT-IS :content ?!what :context ?akrl-context :active-goal ?goal))
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (ONT::ASK-WHAT-IS :content ?!what
								 :context ?akrl-context
								 :active-goal ?goal)))
		     )
	  :destination 'handle-csm-response
	  :trigger t)

	 ;; (not any more) This should go after the previous (-propose-goal-via-question>)
	 (transition
	  :description "ask-if. eg: Does the BRAF-NRAS complex vanish?"
	  :pattern '((ONT::SPEECHACT ?!sa (? s-act ONT::ASK-IF) :what ?!what)
		     (?!spec ?!what ?!type)
		     (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))  
		     (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
		     -ask-question>
		     (RECORD CPS-HYPOTHESIS (ONT::ASK-IF :content ?!what :context ?akrl-context :active-goal ?goal))
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (ONT::ASK-IF :content ?!what
							    :context ?akrl-context
							    :active-goal ?goal)))
		     )
	  :destination 'handle-csm-response
	  :trigger t)
		      
	 (transition
	  :description "conditional ask-if. eg: is ERK activated if we add X"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::ASK-CONDITIONAL-IF :what ?!what :condition ?!test)
		     ;; (ONT::EVENT ?!what ONT::SITUATION-ROOT)
		     ;; (ONT::EVENT ?!test ONT::EVENT-OF-CAUSATION) 
		     (?!sp1 ?!what (? t1 ONT::SITUATION-ROOT
;;; These are DRUM events.  Eventually they will go into a branch in the ontology.
ONT::ACTIVITY
ONT::DEPLETE
ONT::PHOSPHORYLATION
ONT::UBIQUITINATION
ONT::ACETYLATION
ONT::FARNESYLATION
ONT::GLYCOSYLATION
ONT::HYDROXYLATION
ONT::METHYLATION
ONT::RIBOSYLATION
ONT::SUMOYLATION
ONT::PTM
ONT::EXPRESS
ONT::TRANSCRIBE
ONT::TRANSLATE
ONT::HYDROLYZE
ONT::CATALYZE
ONT::ACTIVATE
ONT::PRODUCE
ONT::DEACTIVATE
ONT::CONSUME
ONT::STIMULATE
ONT::INHIBIT
ONT::INCREASE
ONT::DECREASE
ONT::PPEXPT
ONT::BIND
ONT::BREAK
ONT::TRANSLOCATE
ONT::MODULATE
ONT::NO-CHANGE
ONT::TRANSFORM
ONT::SIGNALING
ONT::INTERACT 
						   ))
		     (?!sp2 ?!test (? t2 ONT::EVENT-OF-CAUSATION
ONT::ACTIVITY
ONT::DEPLETE
ONT::PHOSPHORYLATION
ONT::UBIQUITINATION
ONT::ACETYLATION
ONT::FARNESYLATION
ONT::GLYCOSYLATION
ONT::HYDROXYLATION
ONT::METHYLATION
ONT::RIBOSYLATION
ONT::SUMOYLATION
ONT::PTM
ONT::EXPRESS
ONT::TRANSCRIBE
ONT::TRANSLATE
ONT::HYDROLYZE
ONT::CATALYZE
ONT::ACTIVATE
ONT::PRODUCE
ONT::DEACTIVATE
ONT::CONSUME
ONT::STIMULATE
ONT::INHIBIT
ONT::INCREASE
ONT::DECREASE
ONT::PPEXPT
ONT::BIND
ONT::BREAK
ONT::TRANSLOCATE
ONT::MODULATE
ONT::NO-CHANGE
ONT::TRANSFORM
ONT::SIGNALING
ONT::INTERACT 

				    )) 
		     (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
		     (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
		     -propose-test>
		     (RECORD CPS-HYPOTHESIS (ONT::ASK-CONDITIONAL-IF :content ?!what :condition ?!test :context ?akrl-context :active-goal ?goal))
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (ONT::ASK-CONDITIONAL-IF :content ?!what
									:condition ?!test
									:context ?akrl-context
									:active-goal ?goal)))
		     )
	  :destination 'handle-csm-response
	  :trigger t)
		      
	 (transition
	  :description "assertion. eg: Kras activates Raf -- as performing steps in elaborating a model"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::TELL :what ?!root)  ;; we allow intermediate verbs between SA and activate (e.g., KNOW)
		     ;;(ONT::EVENT ?!what ONT::ACTIVATE :agent ?!agent :affected ?!affected)
		     (ont::eval (generate-AKRL-context :what ?!root :result ?akrl-context))
		     (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
		     -refine-goal-with-assertion>
		     (RECORD CPS-HYPOTHESIS (ASSERTION :content ?!root :context ?akrl-context :active-goal ?goal))
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (ASSERTION :content ?!root
							  :context ?akrl-context
							  :active-goal ?goal)))
		     )
	  :destination 'handle-csm-response
	  :trigger t)
	 
	 )
	))

(add-state 'handle-CSM-response
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "CSM returns a successful proposal interpretation"
	  :pattern '((BA-RESPONSE X (? act ADOPT ASSERTION) :what ?!goal :as ?as :context ?new-akrl :alternative ?alt-as)
		     -successful-interp1>
		     (UPDATE-CSM (PROPOSED :content (ADOPT :what ?!goal :as ?as)
				  :context ?new-akrl))
		     (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE-GOAL
						:content (ADOPT :what ?!goal :as ?as)
						:context ?new-akrl))
		     (RECORD ACTIVE-GOAL ?!goal)
		     (RECORD ALT-AS ?alt-as)
		     (RECORD ACTIVE-CONTEXT ?new-akrl)
		     (INVOKE-BA :msg (EVALUATE 
				      :content ((? act ADOPT ASSERTION) :what ?!goal :as ?as)
				      :context ?new-akrl))
		     )
	  :destination 'propose-cps-act-response
	  )

	 ;; failure: can't identify goal
	 ;;(TELL :RECEIVER DAGENT :CONTENT (REPORT :content (FAILED-TO-INTERPRET :WHAT ONT::V32042 :REASON (MISSING-ACTIVE-GOAL) :POSSIBLE-SOLUTIONS (ONT::BUILD-MODEL)) :context ()) :IN-REPLY-TO IO-32505 :sender CSM)
	 (transition
	  :description "CSM fails to identify the goal, but has a guess"
	  :pattern '((BA-RESPONSE  X FAILURE :type FAILED-TO-INTERPRET :WHAT ?!content :REASON (MISSING-ACTIVE-GOAL)
		      :POSSIBLE-RESOLUTION (?!possible-goal) :context ?context)
		     (ont::eval  (extract-goal-description :cps-act ?!possible-goal :context ?context :result ?goal-description :goal-id ?goal-id))
		     -intention-failure-with-guess>
		     (RECORD FAILURE (FAILED-TO-INTERPRET :WHAT ?!content :REASON (MISSING-ACTIVE-GOAL) :POSSIBLE-SOLUTIONS (?!possible-goal) :context ?context))
		     (RECORD POSSIBLE-GOAL ?!possible-goal)
		     (RECORD POSSIBLE-GOAL-ID ?goal-id)
		     (RECORD POSSIBLE-GOAL-CONTEXT ?goal-description)
		     )
	  :destination 'clarify-goal
	  )
		      
	 (transition
	  :description "CSM fails to identify the goal, and no guess. Right now we just prompt the user
                        to identify their goal and forget the current utterance"
	  :pattern '((BA-RESPONSE  X FAILURE :type FAILED-TO-INTERPRET :WHAT ?!content :REASON (MISSING-ACTIVE-GOAL) :context ?context)
		     -intention-complete-failure>
		     (RECORD FAILURE (FAILED-TO-INTERPRET :WHAT ?!content :REASON (MISSING-ACTIVE-GOAL) :context ?context))
		     (RECORD POSSIBLE-GOAL nil)
		     (GENERATE
		      :content (ONT::FAILED-TO-UNDERSTAND-GOAL :content ?!content)
		      :context ?context)
		     )
	  :destination 'propose-cps-act
	  )

	 (transition
	  :description "CSM fails to identify any relevant events in the ASSERTION"
	  :pattern '((BA-RESPONSE  X FAILURE :type FAILED-TO-INTERPRET :WHAT ?!content :REASON (NO-EVENTS-IN-CONTEXT) :context ?context)
		     -intention-failure-noevent>
		     (RECORD FAILURE (FAILED-TO-INTERPRET :WHAT ?!content :REASON (NO-EVENTS-IN-CONTEXT) :context ?context))
		     )
	  :destination 'segmentend  ; process the next pending speech act, if there is one
	  )
	 )
	))


(add-state 'propose-cps-act-response
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "acceptance of an answer to a question (e.g., I will.)"
	  :pattern '((BA-RESPONSE X ACCEPTABLE :what ?!psgoal :context ?!context)
		     ;; (ont::eval (extract-feature-from-act :result (? goal-id ONT::USER ONT::SYS) :expr ?!psgoal :feature :what))
		     (ont::eval (extract-feature-from-act :result (ANSWER :TO ?R) :expr ?!psgoal :feature :as))
		     (ont::eval (find-attr :result ?!popgoal :feature POP-GOAL))
		     (ont::eval (find-attr :result ?!popcontext :feature POP-CONTEXT))
		     -goal-response-q-answered>
		     (UPDATE-CSM (ACCEPTED :content ?!psgoal :context ?!context))
		     (UPDATE-CSM (SET-OVERRIDE-INITIATIVE :OVERRIDE T :VALUE T))  ; system will take initiative
		     (NOTIFY-BA :msg (COMMIT
				      :content ?!psgoal)) ;; :context ?!context))  SIFT doesn't want the context
		     (RECORD POP-GOAL nil)
		     (RECORD POP-CONTEXT nil)
		     (RECORD ACTIVE-GOAL ?!popgoal)
		     (RECORD ACTIVE-CONTEXT ?!popcontext)
		     (GENERATE :content (ONT::ACCEPT))
		     )
	  :destination 'what-next-initiative)
		      
	 (transition
	  :description "acceptance of changing the agent (e.g., You do it.)"
	  :pattern '((BA-RESPONSE X ACCEPTABLE :what ?!psgoal :context ?!context)
		     (ont::eval (extract-feature-from-act :result (? ag ONT::USER ONT::SYS) :expr ?!psgoal :feature :what))
		     (ont::eval (extract-feature-from-act :result (AGENT :OF ?G) :expr ?!psgoal :feature :as))
		     -goal-response-change-performer>
		     (UPDATE-CSM (ACCEPTED :content (ADOPT :what ?G) :context ?!context))
		     (NOTIFY-BA :msg (COMMIT
				      :content (ADOPT :what ?G))) ;; :context ?!context))  SIFT doesn't want the context
		     (UPDATE-CSM (ACCEPTED :content ?!psgoal :context ?!context))
		     (NOTIFY-BA :msg (COMMIT
				      :content ?!psgoal)) ;; :context ?!context))  SIFT doesn't want the context
		     (RECORD ACTIVE-GOAL ?G)
		     (RECORD ACTIVE-CONTEXT ?!context)
		     (GENERATE :content (ONT::ACCEPT))
		     )
	  :destination 'what-next-initiative)

	 (transition
	  :description "acceptance"
	  :pattern '((BA-RESPONSE X ACCEPTABLE :what ?!psgoal :context ?!context)
		     (ont::eval (extract-feature-from-act :result ?goal-id :expr ?!psgoal :feature :what))
		     -goal-response1>
		     (UPDATE-CSM (ACCEPTED :content ?!psgoal :context ?!context))
		     (NOTIFY-BA :msg (COMMIT
				      :content ?!psgoal)) ;; :context ?!context))  SIFT doesn't want the context
		     (RECORD ACTIVE-GOAL ?goal-id)
		     (RECORD ACTIVE-CONTEXT ?!context)
		     (GENERATE :content (ONT::ACCEPT))
		     )
	  :destination 'what-next-initiative)
				
	 (transition
	  :description "acceptance with clarification"
	  :pattern '((BA-RESPONSE X ACCEPT-WITH-CLARIFY :what ?!psgoal :context ?!context :reason ?!reason)
		     (ont::eval (extract-feature-from-act :result ?goal-id :expr ?!psgoal :feature :what))
		     -goal-response-with-clarify>
		     (UPDATE-CSM (ACCEPTED :content ?!psgoal :context ?!context))
		     (NOTIFY-BA :msg (COMMIT
				      :content ?!psgoal)) ;; :context ?!context))  SIFT doesn't want the context
		     (UPDATE-CSM (ACCEPTED :content (ADOPT :what ?!reason :as (SUBGOAL :of ?goal-id)) :context ?!context))
		     (NOTIFY-BA :msg (COMMIT
				      :content (ADOPT :what ?!reason :as (SUBGOAL :of ?goal-id)))) ;; :context ?!context))  SIFT doesn't want the context
		     (RECORD ACTIVE-GOAL ?!reason)   ; we don't need QUERY-GOAL then
		     (RECORD ACTIVE-CONTEXT ?!context)
		     (RECORD QUERY-GOAL ?!reason)
		     (RECORD POP-GOAL ?goal-id)  ; tmp hack
		     (RECORD POP-CONTEXT ?!context)  ; tmp hack
		     (GENERATE :content (ONT::ACCEPT))
		     (GENERATE :content (ONT::QUERY :what ?!reason) :context ?!context)
		     )
	  :destination 'process-user-response-to-question)
				
	 (transition
	  :description "BA rejects the goal (old format -- probably obsolete)"
	  :pattern '((BA-RESPONSE (? x REJECT UNACCEPTABLE) ?!psobj :context ?!context )
		     ;;(ont::eval (find-attr ?goal  GOAL))
		     -goal-response2>
		     (RECORD REJECTED ?!psobj :context ?context)
		     )
	  :destination 'explore-alt-interp)
	 
	 (transition
	  :description "BA fails to identify the goal, and no guess. Right now we just prompt the user
                        to identify their goal and forget the current utterance"
	  :pattern '((BA-RESPONSE  X FAILURE :type CANNOT-PERFORM :WHAT ?!content :REASON ?reason :context ?context)
		     -BA-problem-to-report>
		     (RECORD FAILURE (PROBLEM :type CANNOT-PERFORM
				      :WHAT ?!content :REASON ?reason))
		     (RECORD POSSIBLE-GOAL nil)
		     (UPDATE-CSM (V FAILURE))
		     (GENERATE :content (V FAILURE) :context ?context)
		     )
	  :destination 'process-user-response-to-problem)
	 
	 (transition
	  :description "BA fails to identify the goal, and no guess. Right now we just prompt the user
                        to identify their goal and forget the current utterance"
	  :pattern '((BA-RESPONSE  X FAILURE :type (? x FAILED-TO-INTERPRET CANNOT-IDENTIFY-MOTIVATION)
		      :WHAT ?!content :REASON ?reason :context ?context)
		     -BA-failure>
		     (RECORD FAILURE (FAILED-TO-INTERPRET :WHAT ?!content :REASON ?reason :context ?context))
		     (UPDATE-CSM (V FAILURE))
		     (RECORD POSSIBLE-GOAL nil)
		     )
	  :destination 'explore-alt-interp)
	 )
	))

(add-state 'process-user-response-to-problem
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "goal modification. eg: Let's build a tower instead."
	  :pattern '((ONT::SPEECHACT ?!sa (? x ONT::PROPOSE ONT::REQUEST) :what ?!what)
		     ;; ((? spec ONT::EVENT ONT::EPI) ?!what ?!t)
		     (?!spec ?!what ?!t)
		     (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
		     (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
		     -propose-goal-modify>
		     (RECORD CPS-HYPOTHESIS (PROPOSE :content ?!what :context ?akrl-context :active-goal ?goal))
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (PROPOSE :content ?!what
							:as (MODIFY)
							:context ?akrl-context
							:active-goal ?goal)))
		     )
	  :destination 'handle-csm-response
	  ;; :trigger t
	  )
	 )
	))

(add-state 'process-user-response-to-question
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "13;    me"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::ANSWER :what ?!what)
		     (?!spec ?!what ?!t)
		     (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
		     (ont::eval (find-attr :result ?goal :feature QUERY-GOAL))
		     -answer-atom>
		     (RECORD CPS-HYPOTHESIS (PROPOSE :content ?!what :context ?akrl-context :active-goal ?goal))
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (PROPOSE :content ?!what
							:as (ANSWER :to ?goal)
							:context ?akrl-context
							:active-goal ?goal)))
		     )
	  :destination 'handle-csm-response
	  )
		      
	 (transition
	  :description "I will"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::TELL :what ?!what)
		     (ONT::F ?!what ONT::ELLIPSIS :neutral ?ag)
		     (ONT::PRO ?ag ONT::PERSON :refers-to ?performer)
		     (ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
		     (ont::eval (find-attr :result ?goal :feature QUERY-GOAL))
		     -answer-ellipsis>
		     (RECORD CPS-HYPOTHESIS (PROPOSE :content ?performer :as (ANSWER :to ?goal) :context ?akrl-context :active-goal ?goal))
		     (RECORD QUERY-GOAL nil)
		     (RECORD ACTIVE-GOAL ?goal)
		     (RECORD ACTIVE-CONTEXT ?akrl-context)
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content (PROPOSE :content ?performer
							:as (ANSWER :to ?goal)
							:context ?akrl-context
							:active-goal ?goal)))
		     )
	  :destination 'handle-csm-response)
	 )
	))

(add-state 'explore-alt-interp
 (state :action '(continue)
	:transitions
	(list
	 (transition
	  :description "we have a backup interpretation"
	  :pattern '((continue :arg ?!dummy)
		     (ont::eval (find-attr :result ?!alt :feature ALT-AS))
		     (ont::eval (find-attr :result ?!new-akrl :feature ACTIVE-CONTEXT))
		     -alt-found1>
		     (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE-GOAL :content (ADOPT :what ?!goal :as ?!alt)))
		     (RECORD ALT-AS nil)
		     (INVOKE-BA :msg (EVALUATE 
				      :content ?!alt
				      :context ?!new-akrl))
		     )
	  :destination 'propose-cps-act-response)
	 
	 (transition
	  :description "no backup alt left"
	  :pattern '((continue :arg ?!dummy)
		     (ont::eval (find-attr :result nil :feature ALT-AS))
		     (ont::eval (find-attr :result ?failure :feature FAILURE))
		     -failure-with-no-alt>>
		     (GENERATE :content ?failure)
		     )
	  :destination 'clarify-goal)
	 )
	))

;;  CLARIFICATION MANAGEMENT

(add-state 'clarify-goal 
 (state :action '(GENERATE :content (ONT::CLARIFY-GOAL :content (V possible-goal-id) :context (V POSSIBLE-GOAL-context)))
	:preprocessing-ids '(yes-no)
	:transitions
	(list
	 (transition
	  :description "yes"
	  :pattern '((ANSWER :value YES)
		     (ont::eval (find-attr :result ?context :feature POSSIBLE-GOAL-context))
		     (ont::eval (find-attr :result ?poss-goal :feature possible-goal))
		     -right-guess-on-goal>
		     (INVOKE-BA :msg (EVALUATE 
				      :content ?poss-goal
				      :context ?context))
		     )
	  :destination 'confirm-goal-with-BA)
	 
	 (transition
	  :description "no"
	  :pattern '((ANSWER :value NO)
		     -propose-cps-act>
		     (GENERATE :content (ONT::OK)))
	  :destination 'propose-cps-act)
	 )
	))

(add-state 'confirm-goal-with-BA
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "check with BA that the clarified goal is acceptable"
	  :pattern '((BA-RESPONSE X ACCEPTABLE :what ?!psgoal :context ?!context)
		     (ont::eval (extract-feature-from-act :result ?goal-id :expr ?!psgoal :feature :what))
		     (ont::eval (find-attr :result ?orig-cps-hyp :feature CPS-HYPOTHESIS))
		     (ont::eval (find-attr :result ?active-goal :feature POSSIBLE-GOAL-ID))
		     (ont::eval (replace-feature-val-in-act :result ?new-cps-hyp
				 :act ?orig-cps-hyp :feature :active-goal :newval ?active-goal))
		     -confirmed-clarify-goal>
		     (UPDATE-CSM (ACCEPTED :content ?!psgoal :context ?!context))
		     (RECORD ACTIVE-GOAL ?active-goal)
		     (RECORD CPS-HYPOTHESIS ?new-cps-hyp)
		     (NOTIFY-BA :msg (COMMIT
				      :content ?!psgoal)) ;; :context ?!context))  SIFT doesn't want the context
		     (RECORD ACTIVE-GOAL ?goal-id)
		     (RECORD ACTIVE-CONTEXT ?!context)
		     (GENERATE :content (ONT::EVALUATION :content (ONT::GOOD)))
		     ;;  Now we try to reinterpret the original utterance that caused the clarification
		     (INVOKE-BA :msg (INTERPRET-SPEECH-ACT
				      :content ?new-cps-hyp)))
	  :destination 'handle-CSM-response)
	 )
	))


;; INTITIATIVE MANAGEMENT
;; This state starts an interaction with the BA to determine if the system should take
;; initiative or not

(add-state 'what-next-initiative
 (state :action '(any-pending-speech-acts?)
	:transitions
	(list
	 (transition
	  :description "there's no pending speech act, so we'll ask the CSM"
	  :pattern '((REPLY :content (RESULT :content NO :context ?!c))
		     -take-init1>
		     (nop)
		     )
	  :destination 'what-next-initiative-CSM)

	 (transition
	  :description "there's a pending speech act, end this so it can be processed"
	  :pattern '((REPLY :content (RESULT :content YES :context ?!c))
		     -take-init2>
		     (nop))
	  :destination 'segmentend)
	 )
	))

(add-state 'what-next-initiative-CSM
 (state :action '(take-initiative? :goal (V active-goal) :context (V active-context))
	:transitions
	(list
	 (transition
	  :description "decided on taking initiative"
	  :pattern '((TAKE-INITIATIVE :result YES :goal ?!result :context ?context)
		     -take-init1-csm>
		     (UPDATE-CSM (INITIATIVE-TAKEN-ON-GOAL :what ?!result :context ?context))
		     (INVOKE-BA :msg (WHAT-NEXT :active-goal ?!result
				      :context ?context))
		     )
	  :destination 'perform-BA-request)
	 
	 ;; initiative declined, enter a wait state
	 (transition
	  :description "no initiative"
	  :pattern '((TAKE-INITIATIVE :result NO)
		     -take-init2-csm>
		     (UPDATE-CSM (NO-INITIATIVE-TAKEN)))
	  :destination 'segmentend)
	 
	 ;; failure to interpret goal, just keep going
	 (transition
	  :description "can't understand"
	  :pattern '((FAILURE :what ?!X)
		     -take-init3-csm>
		     (UPDATE-CSM (NO-INITIATIVE-TAKEN)))
	  :destination 'segmentend)
	 )
	))

(add-state 'perform-BA-request
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "failed trying to achieve the goal"
	  :pattern '((BA-RESPONSE X FAILURE :what ?!F1 :as ?as-role :context ?context)
		     -failed1>
		     (UPDATE-CSM (FAILED-ON  :what ?!F1 :as ?as-role :context ?context))
		     (GENERATE 
		      :content (ONT::TELL :content (ONT::FAIL :formal ?!F1 :tense PAST))
		      :context ?context)
		     )
	  :destination 'segmentend)
				
	 (transition
	  :description "solution to goal reported"
	  :pattern '((BA-RESPONSE X SOLUTION :what ?!what :goal ?goal :context ?akrl-context)
		     ;;(ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
		     -soln1>
		     (UPDATE-CSM (SOLVED :what ?!what :goal ?goal :context ?akrl-context))
		     (GENERATE 
		      :content (ONT::TELL :content ?!what)
		      :context ?akrl-context)
		     )
	  :destination 'segmentend)
				
	 ;; initiative declined, enter a wait state
	 (transition
	  :description "BA has nothing to do"
	  :pattern '((BA-RESPONSE ?!x WAIT)
		     -wait>
		     (UPDATE-CSM (BA-WAITING)))
	  :destination 'segmentend)
				
	 (transition
	  :description "suggestion of user action"
	  :pattern '((BA-RESPONSE ?!X PERFORM :agent *USER* :action ?!action :context ?context)
		     (ont::eval (find-attr :result ?goal :feature ACTIVE-GOAL))
		     -what-next1>
		     (UPDATE-CSM (PROPOSED :content (ADOPT :what ?!action :as (SUBGOAL :OF ?goal)) :context ?context))
		     (RECORD ACTIVE-GOAL ?!action)
		     (RECORD ACTIVE-CONTEXT ?context)
		     (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE :content (ADOPT :what ?!action :as (SUBGOAL :OF ?goal)) :context ?context))
		     (GENERATE
		      :content (ONT::PROPOSE :content (ONT::PERFORM :action ?!action :context ?context)))
		     )
	  :destination 'proposal-response)

	 (transition
	  :description "action completed!"
	  :pattern '((BA-RESPONSE ?!X EXECUTION-STATUS :action ?!action :status ONT::DONE)
		     -what-next5>
		     (UPDATE-CSM (ACTION-COMPLETED :action ?!action))
		     (GENERATE :content (ONT::EVALUATION :content (ONT::GOOD)))
		     )
	  :destination 'what-next-initiative)
				
				
	 (transition
	  :description "action completed!"
	  :pattern '((BA-RESPONSE ?!X GOAL-ACHIEVED)
		     -what-next3>
		     (UPDATE-CSM (GOAL-ACHIEVED))
		     (GENERATE :content (ONT::EVALUATION :content (ONT::GOOD)))
		     (GENERATE :content (ONT::CLOSE))
		     )
	  :destination 'segmentend)
	 
	 (transition
	  :description "answer to a question"
	  :pattern '((BA-RESPONSE X ANSWER :what ?!what :of ?of :goal ?goal :justification ?j :context ?akrl-context)
		     ;;(ont::eval (generate-AKRL-context :what ?!what :result ?akrl-context))
		     -answer>
		     (UPDATE-CSM (ANSWER :what ?!what :of ?of :goal ?goal :justification ?j :context ?akrl-context))
		     (GENERATE 
		      :content (ONT::ANSWER :content (?!what :of ?of :goal ?goal :justification ?j :context ?akrl-context)))
		     )
	  :destination 'what-next-initiative)
	 )
	))

;;;;
;;    Here are the acts starting a dialogue with system intitative


;;  Setting CPS GOALS  -- System Initiative
;;  This assume that the CPS state has a preset proviate system goal that
;; we want to make into a shared goal with the user
;; N ote:: this assumes the private system goal is already cached for processing
(add-state 'initiate-CPS-goal
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "we don't have a private goal, we ask the user"
	  :pattern '((CSM-RESPONSE ?!x PRIVATE-SYSTEM-GOAL :content (IDENTIFY :neutral ?what :as ?as)
		      :context ?context)
		     -prompt-user-goal>
		     ;;(RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE-GOAL :what ?!content :context ?context))
		     (GENERATE
		      :content (QUERY :what ?what :as ?as)
		      :context ?context)
		     )
	  :destination 'propose-cps-act)
	 
	 (transition
	  :description "we know the private goal, so we propose it to the user"
	  :pattern '((CSM-RESPONSE ?!x PRIVATE-SYSTEM-GOAL :content ?!content :context ?context)
		     -propose-sys-goal>
		     (RECORD PROPOSAL-ON-TABLE (ONT::PROPOSE-GOAL :what ?!content :context ?context))
		     (GENERATE
		      :content (ONT::PROPOSE-GOAL :content ?!content :context ?context))
		     )
	  :destination 'initiate-csm-goal-response)
	 )
	))

(add-state 'initiate-csm-goal-response
 (state :action nil
	:implicit-confirm t
	:transitions
	(list
	 ;; If the user rejects this, we ask them to propose something
	 (transition
	  :description "rejectance"
	  :pattern '((ONT::SPEECHACT ?sa1 ONT::REJECT )
		     -intitiate-response2>
		     (UPDATE-CSM (REJECTED :content ?!content :context ?!context))
		     (GENERATE
		      :content (ONT::REQUEST :content (ONT::PROPOSE-GOAL :agent *USER*))))
	  :destination 'segmentend)
	 
	 (transition
	  :description "acceptance"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::ACCEPT)
		     (ont::eval (find-attr :result (?prop :what ?!content :context ?!context) 
				 :feature PROPOSAL-ON-TABLE))
		     -initiate-response1>
		     (UPDATE-CSM (ACCEPTED :content ?!content :context ?!context))
		     (NOTIFY-BA :msg (COMMIT
				      :content ?!content)) ;; :context ?!context))  SIFT doesn't want the context
		     (RECORD ACTIVE-GOAL ?!content)
		     (RECORD ACTIVE-CONTEXT ?!context)
		     (NOTIFY-BA :msg (SET-SHARED-GOAL
				      :content ?!content
				      :context ?!context))
		     )
	  :destination 'what-next-initiative)
	 )
	))

(add-state 'user-prompt
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "what next"
	  :pattern '((ONT::WH-TERM ?!sa ONT::REFERENTIAL-SEM :proform ont::WHAT)
		     ;; ((? sp ONT::F ONT::EVENT) ?s1 ONT::SEQUENCE-VAL)
		     (ONT::F ?s1 ONT::SEQUENCE-VAL)
		     -what-next>
		     (INVOKE-BA :msg (WHAT-NEXT :active-goal ?!result
				      :context ?context))
		     )
	  :destination 'perform-BA-request
	  :trigger t)
	 
	 ;; If the user rejects this, we ask them to propose something
	 (transition
	  :description "what should we do next?"
	  :pattern '((ONT::SPEECHACT ?sa1 ONT::S_WH-QUESTION :focus ?!foc)
		     (ONT::WH-TERM ?!sa ONT::REFERENTIAL-SEM :proform ont::WHAT)
		     -what-next2>
		     (INVOKE-BA :msg (WHAT-NEXT :active-goal ?!result
				      :context ?context))
		     )
	  :destination 'perform-BA-request)
	 )
	))

				
(add-state 'proposal-response
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "OK/ accept"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::ACCEPT)
		     (ont::eval (find-attr :result (?prop :content ?!content :context ?!context)  
				 :feature PROPOSAL-ON-TABLE))
		     -proposal-response1>
		     (UPDATE-CSM  (ACCEPTED :content ?!content :context ?!context))
		     (NOTIFY-BA :msg (COMMIT
				      :content ?!content)) ;; :context ?!context))  SIFT doesn't want the context
		     (NOTIFY-BA :msg (NOTIFY-WHEN-COMPLETED :agent *USER*
				      :content ?!content
				      :context ?!context))
		     )
	  :destination 'expect-action)
				
	 (transition
	  :description "action completed (from BA)"
	  :pattern '((EXECUTION-STATUS :action ?!act :status ont::DONE)
		     -demonstrate-action1>
		     (RECORD ACTIVE-GOAL ?!act)
		     )
	  :destination 'what-next-initiative)
	 
	 (transition
	  :description "I can't do it"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::TELL :what ?e)
		     (ONT::F ?e ONT::EXECUTE :force (? x IMPOSSIBLE))
		     (ont::eval (find-attr :result (?prop :content ?!content :context ?!context)  
				 :feature PROPOSAL-ON-TABLE))
		     -inability>
		     (UPDATE-CSM (FAILED-ON :what ?!content :context ?!context))
		     )
	  :destination 'what-next-initiative)
				
	 (transition
	  :description "you do it"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::TELL :what ?e)
		     (ONT::F ?e ONT::EXECUTE :agent ?ag :force ont::TRUE)
		     (ONT::PRO ?ag ONT::PERSON :refers-to ONT::SYS)
		     (ont::eval (find-attr :result (?prop :content ?!content :context ?!context)  
				 :feature PROPOSAL-ON-TABLE))
		     (ont::eval (extract-feature-from-act :result ?goal-id :expr ?!content :feature :what))
		     -change-performer>
		     (UPDATE-CSM (PROPOSED
				  :content (ADOPT :what ONT::SYS :as (AGENT :of ?goal-id))
				  :context ?!context))
		     (INVOKE-BA :msg (EVALUATE 
				      :content (ADOPT :what ONT::SYS :as (AGENT :of ?goal-id))
				      :context ?!context))
		     )
	  :destination 'propose-cps-act-response)
	 )
	))

;; here we are waiting for something to happen in the world

(add-state 'expect-action
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "action completed (from BA)"
	  :pattern '((EXECUTION-STATUS :action ?!act :status ont::DONE)
		     -demonstrate-action2>
		     (RECORD LAST-ACTION-DONE ?!act)
		     )
	  :destination 'what-next-initiative)
	 
	 ;;  user might speak while we're waiting for confirmation
	 (transition
	  :description "what next"
	  :pattern '((ONT::WH-TERM ?!sa ONT::REFERENTIAL-SEM :proform ont::WHAT)
		     ;; ((? sp ONT::F ONT::EVENT) ?s1 ONT::SEQUENCE-VAL)
		     (ONT::F ?s1 ONT::SEQUENCE-VAL)
		     -what-next_b>
		     (nop))
	  :destination 'what-next-initiative)
								
	 ;; OK might have come in after the action was performed
	 (transition
	  :description "OK/ accept"
	  :pattern '((ONT::SPEECHACT ?!sa ONT::ACCEPT)
		     -OK-confirm-done>
		     (NOP)
		     )
	  :destination 'what-next-initiative)
	 
	 (transition
	  :description "ONT::OK from BA"
	  :pattern '((ONT::OK)
		     -confirm>
		     (NOP))
	  :destination 'expect-action)
	 )
	))

(add-state 'done
 (state :action nil
	:transitions
	(list
	 (transition
	  :description "goal accomplished"
	  :pattern '((ONT::F ?!v ONT::HAVE-PROPERTY :FORMAL ?!f)
		     (ONT::F ?!f ONT::FINISHED)
		     -done1>
		     (UPDATE-CSM (GOAL-ACHIEVED))
		     (GENERATE :content (ONT::EVALUATION :content (ONT::GOOD)))
		     (GENERATE :content (ONT::CLOSE)))
	  :destination 'segmentend
	  :trigger t)				
	 )
	))
