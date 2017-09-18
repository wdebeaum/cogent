;;  Managing the collaborative problem solving state
;;  This is sill hacked together to support the demos -- but the interfaces are correct

(in-package :dagent)

(defvar *external-CSM* t)  ;; set to t if we are using a separate CSM module

(defun set-system-goal (&key id what context)
  (let* ((user (lookup-user 'desktop))
    ;; eventually get rid of the next two and do within the model
    ;;(set-CPS-variable :current-shared-goal content)
    ;;(set-CPS-variable :system-context context)
	 (goal ;;(if (equal content '(IDENTIFY :agent ONT::USER :what WH-TERM :as (GOAL)))
		 ;;  '(PRIVATE-SYSTEM-GOAL :content (IDENTIFY :agent ont::USER  :what X :as (GOAL))
		   (list 'PRIVATE-SYSTEM-GOAL :id id :what what :context context)))
		    
    ;(update-csm goal)
    (setf (user-time-of-last-user-interaction user) (get-time-of-day))
      (cache-response-for-processing (list (list* 'CSM-RESPONSE 'XXX goal)))
      (invoke-state 'initiate-CPS-goal nil user nil nil))
    )

;;;  Here's the code that manages the interface to the CSM
  
(defun query-csm (&key result content)
  (cache-response-for-processing (list (get-csm-response `(REQUEST :content (QUERY-CSM :content ,content)))))
  )

#||(defun find-CSM-interps (&key sa what result context new-akrl-context test active-goal)
  (let* ((realgoal (if (consp active-goal) (find-arg-in-act active-goal :what)
		       active-goal))
	 (speechact (if test
			`(INTERPRET-SPEECH-ACT :content (,sa :content ,what :context ,context :test ,test :active-goal ,realgoal))
			`(INTERPRET-SPEECH-ACT :content (,sa :content ,what :context ,context :active-goal ,realgoal))))
	 (reply  (get-csm-response `(REQUEST :content ,speechact)))
	 (result-value (find-arg-in-act reply :content))
	 (new-akrl-context-value (find-arg-in-act reply :context)))
    (append (im::match-vals nil result result-value)
	    (im::match-vals nil new-akrl-context new-akrl-context-value)))
  )||#

(defun take-initiative? (&key result goal context)
  (let* ((realgoal (if (consp goal) (find-arg-in-act goal :id) goal))
	 (reply (get-csm-response `(REQUEST :content (take-initiative? :goal ,realgoal :context ,context))))
	 )
    reply))

(defun any-pending-speech-acts? (&key result goal context)
  (send-and-wait `(REQUEST :content (any-pending-speech-acts))))

(defun get-csm-response (msg)
  (if *external-CSM*
      (send-and-wait msg)
      (process-csm-msg (find-arg-in-act msg :content))))

(defun update-csm (update)
  (if *external-CSM*
      (multiple-value-bind
	    (content context)
	  (separate-context update)
	(send-msg `(REQUEST :content (UPDATE-CSM :content ,content :context ,context))))
    ;(do-csm-update update)
    (process-csm-msg (list 'UPDATE-CSM :content update))
    ))
      
(defun separate-context (term)
  (values (cons (car term) (remove-arg (cdr term) :context))
	  (find-arg-in-act term :context)))

(defun invoke-csm (act)
  ;; this is the equivalent of INVOKE-BA -- which calls the module and then
  ;;  caches the response so it is picked up at the next state transition
  ;;  currently only is used for INTERPRET-SPEECH-ACT
  (let* ((reply (process-csm-msg (find-arg-in-act act :msg)))
	 (content (find-arg-in-act reply :content))
	 (context (find-arg-in-act reply :context))
	 (converted-response (append (list* 'BA-RESPONSE 'X (car reply) :psact content) (list :content content :context context))))
    (if content (cache-response-for-processing (list converted-response))
	(format t "~%WARNING: CSM returned uninterpretable response: ~S:" reply))))

;;  Here is the code for the built-in CSM when it is being used

(defun process-csm-msg (msg)
    (case (car msg)
      (query-csm
       (do-csm-query (find-arg-in-act msg :content)))
      (interpret-speech-act
       (do-csm-interp (find-arg-in-act msg :content)))
      (take-initiative? 
       (do-csm-initiative (find-arg-in-act msg :goal) (find-arg-in-act msg :context)))
      (update-csm
       (do-csm-update (find-arg-in-act msg :content)))
      (otherwise
       (format t "~%error: unknown CSM request: ~S" msg))))


;;  HERE ARE THE MAIN FUNCTIONS TO IMPLEMENT WITH THE EMBEDDED CSM HANDLER

(defun do-csm-update (update)
  ;;  update the CSM, no need to return anything
  ;; but should update the state in order to manage goal tree etc
  (format t "~%CSM update in: ~S~%" update)
  )

(defun do-csm-query (query)
  ;; returns comment in a REPLY msg format
  ;;  currently this is only called with the query parameter ACTIVE-GOAL but I expect we'll expand this later
  (format t "~%CSM query in: ~S~%" query)
  (case (car query)
    (active-goal
     (setq output 
     '(REPORT :CONTENT (ACTIVE-GOAL :ID C00002 :WHAT ONT::V33196) :CONTEXT
     ((ONT::RELN ONT::V33310 :INSTANCE-OF ONT::ASSOC-WITH :FIGURE ONT::V33262
       :GROUND ONT::V33245 :FORCE ONT::TRUE)
      (ONT::A ONT::V33262 :INSTANCE-OF ONT::STAIRS :MOD ONT::V33310 :LEX
       W::STAIRCASE)
      (ONT::RELN ONT::V33196 :INSTANCE-OF ONT::CREATE :AGENT ONT::V33287
       :AFFECTED-RESULT ONT::V33262 :TENSE W::PRES :VFORM W::BASE :FORCE
       ONT::TRUE :LEX W::BUILD))))
     )
    )

  (format t "~%CSM query out: ~S~%" output)
  output

  )

(defun do-csm-interp (speechact)
  ;; This does the INTERPRET-SPEECH-ACT function,  returns content in a REPLY msg format
  ;;   (REPORT :content (ADOPT :id ...)

  (format t "~%CSM interpret-speech-act in: ~S~%" speechact)

  (let* ((content (find-arg-in-act speechact :content))
	 (context (find-arg-in-act speechact :context))
	 (active-goal (find-arg-in-act speechact :active-goal))
	 (act-lf (find-lf-in-context content context))
	 (act (find-arg act-lf :instance-of)))
    (case (car speechact)
      (PROPOSE
       (if (parser::subtype-check act 'ONT::CREATE)
	   (progn
	     (setq output `(REPORT :content (ADOPT :ID ,(gentemp "C" 'ONT) :what ,content
						:AS (GOAL))
				:context ,context))
	  nil
	  )
	 )
       )
      (otherwise
       (format t "~%Error: can't interp ~S" speechact)
       (setq output `(REPORT :content (FAILURE))))
      
      )
    )

  #|
  (let* ((context (find-arg-in-act speechact :context))
	 (act (find-lf-in-context (find-arg-in-act speechact :content) context)))
    (case (find-arg act :instance-of)
      (ont::create 
       (setq output `(REPORT :content (ADOPT :ID C00002 :what ,(second act) 
				:AS (GOAL))
		:context ,context)))
      (otherwise
       (format t "~%Error: can't interp ~S" speechact)
       (setq output `(REPORT :content (FAILURE))))
      ))
  |#

  (format t "~%CSM interpret-speech-act out: ~S~%" output)
  output
  
  )
  
(defun do-csm-initiative (goal context)
  ;; This handles the TAKE-INITIATIVE queries
 
  (format t "~%CSM take-initiative in: ~S; ~S~%" goal context)

  (setq output
  '(TAKE-INITIATIVE :result MAYBE :GOAL C00002 :CONTEXT
     ((ONT::RELN ONT::V33310 :INSTANCE-OF ONT::ASSOC-WITH :FIGURE ONT::V33262
       :GROUND ONT::V33245 :FORCE ONT::TRUE)
      (ONT::A ONT::V33262 :INSTANCE-OF ONT::STAIRS :MOD ONT::V33310 :LEX
       W::STAIRCASE)
      (ONT::RELN ONT::V33196 :INSTANCE-OF ONT::CREATE :AGENT ONT::V33287
       :AFFECTED-RESULT ONT::V33262 :TENSE W::PRES :VFORM W::BASE :FORCE
       ONT::TRUE :LEX W::BUILD))))

  (format t "~%CSM take-initiative out: ~S~%" output)
  output

  )
