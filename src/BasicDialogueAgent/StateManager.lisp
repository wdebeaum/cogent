;;  Basic Dialogue Agent State Management

(in-package :dagent)
  
;;(defvar *state-definitions* nil)
(defvar *segments* nil)
(defvar *state-table* nil)
(defvar *transition-ids* nil)
(defvar *starting-segment* nil)
(defvar *preprocessing-rules* nil)
(defvar *users* nil)   ;; the list of defined users
(defvar *active-users* nil)  ;; users who we will be expecting to interact with (i.e., not all the testing users!)
(defvar *current-user* nil) ;; if we need to lookup current user information in a rule, we have it here

(defvar *silent-failures* nil)
(defvar *using-alarms* t)
(defvar *term-extraction-rules* nil) ;;  will be set to rules that interpret the noun phrases if desire

;; variables for maintaining current state
(defvar *saved-text* "")   ;; saved text to be used in mext message
(defvar *words* nil) ;; temporary remembering the words for the current utterance
(defvar *most-recent-lfs* nil)  ;; and the lfs
(defvar *most-recent-result* nil) ; the match-result value for the successful rule match
;; Setting for Wizard interaction - default is no wizard
(defvar *disabled-wizard* t)
(defvar *wizard-prompt-delay-time*  1)  ;; prompt wizard if no interaction in over X minutes
(defvar *wizard-responses* nil)
(defvar *max-wait-for-wizard-reply* 100000)  ;; number of minutes we wait for wizard before continuing on anyway
(defvar *reask-limit* 1)  ;; number of times we reask a question before giving up
;; Timing controls to manage the dialogue 
(defvar *delay-to-clear* 720) ;; if we don't hear from the user in over X minutes, we reset the dialogue state
(defvar *max-wait-until-prompt-in-hours* 4)  ;; if we don't hear from the user for X hours, we prompt.
(defvar *max-wait-until-prompt-in-minutes* (* *max-wait-until-prompt-in-hours* 60))
(defvar *max-wait-until-prompt-in-seconds* (* *max-wait-until-prompt-in-minutes* 60))
(defvar *wait-after-inhaler* 0.333333)  ;; check with the user X hours after inhaler use or suggesting inhaler use (20 minutes)

(defvar *last-internal-response* nil)   ;; this variable is used to store input to the state manager (e.g., a message from an external component like the BA, or a response to a function call within DAGENT.

(defvar *interpretable-utt* nil)

(defun cache-response-for-processing (x)
  (push x *last-internal-response*))

(defun pop-response-cache nil
  (pop *last-internal-response*))


;;  Segments have patterns that are triggered by user input and initiate a state to process the input
(defstruct segment
  trigger     
  preprocessing-ids
  start-state
  derived-from-rule
  id)

;; The core STATE DATA structure

(defstruct state
   action  ;; typically list of questions to pick randomly from for the system to say when it transitions to this state
   explicit-form   ;; 
   preprocessing-ids   ;; a list of proprocessing states that are invoked to interpret the input before checking transitions
   immediate-transition  ;; if T we immediately exit this state after executing the actions
   transitions           ;; a list of possible transitions (pattern/transition forms defined below)
   id                    ;; the state ID (used for indexing, tracing an debugging)
   implicit-confirm      ;; if set to T, we can exit this state without penalty if the utterance doesn't match
   triggered             ;; if T, this state was triggered by a pattern so we deal witure differently
   failure-actions)      ;;  a set of actions to perform on utterance failure (default is wizard or "I don't understand")

(defvar *empty-state* (make-state :id 'empty))

(defstruct transition
  pattern
  output
  destination
  tests
  description
  trigger)

(defstruct rule
  description
  pattern)

(defstruct user
  name        
  channel-id          ; the (SMS or other) channel id for this user
  wizard              ; wizard ID - currently only one
  parent-channel-id   ; parent email or cell # for reports   (ASMA specific)
  daily-report-to     ; additional email or cell # to send reports to
  current-dstate      ; current state for interaction
  dstate-args         ; dynamic arguments set when state is invoked
  local-kb            ; attribute-value pairs for the current user session
  history-kb          ; history of user session
  alarms              ; list of all the preset alarms for the user
  pending-alarms      ; list of alarms that have not been addressed yet (because agent was doing something else)
  medications         ; list of medications (ASMA specific)
  other               ; available for storing other info as needed in attribute-value pair format
  ;; here are various timing records to manage each users dialogue
  (time-of-last-user-interaction (list 0 0)) 
  (time-of-last-wizard-interaction (list 0 0))
  (time-of-last-BA-interaction (list 0 0))
  (wizard-response-pending nil)
  (wizard-controlling-dialogue nil)
  (immediately-prior-turns-that-failed 0)
  role 
  transcript   ;; record of interction to ease debugging
  )

(defun save-pending-alarm (user msg)
  (trace-msg 3 "~%Putting alarm on pending list: ~S ~%  for user ~S" msg user)
  (when (eq (car msg) 'alarm)
    (setf (user-pending-alarms user) (append (user-pending-alarms user)
					     (list msg)))))
(defun add-to-local-kb (feature value user)
  (setf (user-local-kb user)
	(replace-feature-value feature value (user-local-kb user))))

(defun replace-feature-value (f v kb)
  (if (null kb)
      (list (list f v))
      (if (eq (caar kb) f)
	  (if (not (eq (cadar kb) v))
	      (cons (list f v)
		    (cdr kb))
	      kb)
	  (reuse-cons (car kb)
		      (replace-feature-value f v (cdr kb))
		      kb))))

(defun record-transcript (user type data)
  (when (user-p user)
    (push (list type data) (user-transcript user))))

(defun update-prior-failures (user)
  (setf (user-immediately-prior-turns-that-failed user) (+ (user-immediately-prior-turns-that-failed user) 1))
  )

(defun record-success (user)
  (setf (user-immediately-prior-turns-that-failed user) 0)
 )

(defun wizard-channel (user)
  (let ((wiz (lookup-user (user-wizard user))))
    (if (user-p wiz)
	(user-channel-id wiz))))

(defun email-channel-p (channel-id)
  "Is the channel ID for an email address? (is it a string starting with
   'mailto:'?)"
  (and (stringp channel-id)
       (< 7 (length channel-id))
       (string= "mailto:" (subseq channel-id 0 7))))

(defun send-message-to-parent (user subject content &optional cc)
  (declare (type user user)
	   (type string subject content))
  "Send a message to the parent of the given user. If we're using email, use
   subject as the subject line of the email; otherwise ignore it. If cc is
   supplied, always send an email also to that address."
  (let* ((channel-id (user-parent-channel-id user))
         (daily-report-to (user-daily-report-to user))
	 (email-drts (mapcar (lambda (s) (subseq s 7)) ; remove "mailto:"
			     (remove-if-not #'email-channel-p daily-report-to)))
	 (cell-drts (remove-if #'email-channel-p daily-report-to))
	 (combined-cc (if cc (cons cc email-drts) email-drts)))
    (when combined-cc
      (setf combined-cc (format nil "~{~a~^, ~}" combined-cc)))
    (cond
      ((email-channel-p channel-id)
	(send-msg `(request :content
	    (send-mail :to ,(subseq channel-id 7) ; remove "mailto:"
		       ,@(when combined-cc `(:cc ,combined-cc))
		       :subject ,subject
		       :content ,content)))
	(when cell-drts ; if there are cell daily-report-tos, also send text
	  (dolist (ch cell-drts)
	    (send-msg `(request :content
		(say :channel ,ch :content ,content)))))
	)
      (t
        (dolist (ch (cons channel-id cell-drts))
	  (send-msg `(request :content
	      (say :channel ,ch :content ,content))))
	(when combined-cc ; if there's a cc, also send that email
	  (send-msg `(request :content
	      (send-mail :to ,combined-cc :subject ,subject :content ,content))))
	)
      )))

(defstruct alarm
  time
  test
  args
  start-state
  persistent  ;; if T then system will reask for the information again until it gets if even after alarm has passed
  (last-asked (list 0 0))
  )

(defun segment (&key start-state trigger preprocessing-ids)
  (if trigger
    (let ((id (first-atom trigger)))
      (im::add-im-rule trigger id)
      (make-segment :trigger id :preprocessing-ids preprocessing-ids
		    :start-state start-state))
    (make-segment :start-state start-state)))

(defun state (&Key action preprocessing-ids immediate-transition transitions implicit-confirm)
  (make-state :action action
	      :preprocessing-ids preprocessing-ids
	      :transitions transitions
	      :immediate-transition immediate-transition
	      :implicit-confirm implicit-confirm
	      ))

(defun transition (&key pattern destination description trigger)
  (let ((id (first-atom pattern))
	(vpat (im::read-expression pattern)))
    (push id *transition-ids*)
    (im::add-im-rule pattern id)
    (make-transition :description description
		     :pattern id
		     :output (after-first-atom vpat)
		     :tests (mapcar #'second (remove-if-not #'(lambda (x) (and (consp x) (eq (car x) 'ont::EVAL)))
					   vpat))
		     :trigger trigger
		     :destination destination)))

(defun remember-result (res)
  (setq *most-recent-result* res))

;;  here we have to duplicate some of the IM matching capabilities to handle cases when
;;  the answer comes from the wizard. In this case, we're not doing LF matching
;;   but need to invoke the ONT::EVAL tests to get bindings for variables in the OUTPUT spec.

(defun apply-transition-tests (tests bndgs)
  (if tests
    (let* ((next (car tests))
	   (newbndgs (apply (car next) (cdr next))))
      (trace-msg 2 "~% EVAL ~S returns ~S" (car tests) newbndgs)
      (if newbndgs
	  (apply-transition-tests (cdr tests) (im::add-to-binding-list bndgs newbndgs))))
    bndgs))
	  
(defun rule (&Key description id pattern result)
  (let ((id (first-atom pattern)))
    (push id *transition-ids*)
    ;;(im::add-im-rule pattern id)
    (make-rule :description description
	       :pattern pattern)))

(defun alarm (&key time test start-state persistent args)
  (make-alarm :time time
	      :test test
	      :args args
	      :start-state start-state
	      :persistent persistent))
	
(defun first-atom (elements)
  (find-if #'symbolp elements))

(defun after-first-atom (ll)
  (when ll
    (if (symbolp (car ll))
	(cdr ll)
	(after-first-atom (cdr ll)))))

(defun set-starting-segment (x)
  (setq *starting-segment* x))

(defun add-segment (name seg)
  (setf (segment-id seg) name)
  (setq *segments* (append *segments* (list (cons name seg)))))


(defun add-preprocessing (name rules)
  (push (cons name rules) *preprocessing-rules*)
  (mapcar #'(lambda (x)
	      (let ((result (last (rule-pattern x))))
		;; make sure it has an odd number of items
		(when (evenp (list-length result))
		  (format t "~% Warning: result clause in preprocessing rule must be an atom followed by feature value pairs: e.g., (result :f1 va :f2 v2)"))
		(im::add-im-rule (rule-pattern x) name)))
	  rules))

(defun find-segment (name)
  (cdr (assoc name *segments*)))

(defun add-state (name state)
  (setf (state-id state) name)
  (add-state-triggers name (state-transitions state))
  (push (cons name state) *state-table*))

(defun add-state-triggers (id transitions)
  (when transitions
    (when (transition-trigger (car transitions))
      (add-segment id (make-segment :trigger (transition-pattern (car transitions))
				    :start-state (transition-destination (car transitions))
				    :derived-from-rule t)))
    (add-state-triggers id (cdr transitions))))

(defun find-state (name)
  (cdr (assoc name *state-table*)))

(defun restart-dagent nil
  (mapcar #'reset-user (mapcar #'cdr *users*))
  (setq *interpretable-utt* nil)
  (send-status 'OK)
  )

(defun reset-user (user)
  (setf (user-current-dstate user) nil)
  (setf (user-dstate-args user) nil)
  (setf (user-local-kb user) nil)
  (setf (user-history-kb user) nil)
  (setf (user-pending-alarms user) nil)
  (setf (user-transcript user) nil))
  
  
(defun reset-states nil
  (setq *segments* nil)
  (setq *state-table* nil)
  (setq *starting-segment* nil)
  (when *transition-ids* 
    (im::reset-im-rules *transition-ids*))
  (setq *transition-ids* nil))
  
(defun define-user (name user)
    (declare (type string name))
  (if (user-p user)
      (if (user-channel-id user)
	  (progn 
	    (if (null (user-name user))
		(setf (user-name user) name))
	    (push (cons (user-channel-id user) user) *users*)
	    (push (cons name user) *users*))
	  (da-warn "error: user ~S defined without a channel" name))
      (da-warn "bad attempt to define user ~S: ~S" name user)))
	     
(defun lookup-user (id)
  (or (cdr (assoc id *users* :test #'string-equal))
      (cdr (assoc id *users*))))

(defun initialize nil
  (trace-msg 3 "System starting at ~S with users ~S" (get-time-of-day) *active-users*)
  (mapcar #'newday *active-users*)
  nil)

(defun current-dstate (user)
  (let ((dstate 
	 (or (if user (car (user-current-dstate user)))
	     (let ((seg (find-segment *starting-segment*)))
	       (if seg (find-state (segment-start-state seg)))))))
    dstate
    ))


(defun update-current-dstate (state push? user args &optional triggered)
  "we keep track of triggered states because they are handled differently in Wizrd interactions (e.g., how we handle IGNORE)"
  (when user
   ;; (if (state-p state) state   ;; seems like a noop to me
   ;;	(find-state state))
    (trace-msg 2 "Updating current state to ~S for user ~S with args ~S: push? is ~S"
	       (if (state-p state) (state-id state))
	       (user-channel-id user) args push?)
    (if (not (state-p state))
	(setq state (find-state state)))
    (if state 
	(if push? 
	    (push state (user-current-dstate user))
	    ; if not push, we replace the top state
	    (setf (user-current-dstate user) 
		  (cons state (cdr (user-current-dstate user)))))
	(setf (user-current-dstate user) nil))
    (if (and (state-p state) triggered)
	(setf (state-triggered state) triggered))
    ;; report status
    (send-status `(OK :in-state ,(if (state-p state) (state-id state))))
    (trace-msg 2 "STATE STACK is ~S" (mapcar #'state-id (user-current-dstate user)))
    (setf (user-dstate-args user) args)))

(defun pop-current-dstate (user)
  (pop (user-current-dstate user))
  (if (user-current-dstate user)
      (trace-msg 2 "Popping current dstate: new top state is ~S" (state-id (car (user-current-dstate user))))
      (trace-msg 2 "current dstate is NIL"))
  ;; report status
  (let ((state (current-dstate user)))
    (send-status `(OK :in-state ,(if (state-p state) (state-id state)))))
  (user-current-dstate user))

(defun go-to-restart-state (user)
  ;; report status
  (send-status '(OK :in-state nil))
  (setf (user-current-dstate user) nil))

(defun record-for-current-user (act user)
  (if (> (list-length act) 2)
      (let ((feature (cadr act))
	    (value (instantiate-dstate-args (caddr act) user)))
	(trace-msg 3 "Setting values ~S for user ~S" (cdr act) (user-channel-id user))
	(when (or (symbolp value) (consp value) (numberp value))
	    (add-to-local-kb feature value user)
	     
	    (list feature value)))  ;; return this just for tracing
      (da-warn "Bad attribute-value pair in ~S" act)))

(defun instantiate-dstate-args (value user)
  "replace terms that refer to variables in the user dstate with their values:
     we look both in the user's local-kb and in the dstate-args"
  (when value
    (if (consp value)
	(cond 
	  ((and (consp (car value)) (eq (caar value) 'V))
	   (cons (or (cadr (assoc (cadar value) (user-local-kb user)))
		     (find-arg (user-dstate-args user) (cadar value))
		      )
		  (instantiate-dstate-args (cdr value) user)))
	  ((and (consp (car value)) (eq (caar value) 'S))
	   (cons (format nil "~:(~A~)" (cadar value))  ;; write out string in all uppercase
		 (instantiate-dstate-args (cdr value) user)))
	  (t (cons (instantiate-dstate-args (car value) user)
		   (instantiate-dstate-args (cdr value) user))))
	value)))
		    

(defun get-attr (user attr)
  (if (user-p user)
      (get-from-kb (user-local-kb user) attr)))

(defun get-from-kb (kb attr)
  (cadr (get-attr-from-kb kb attr)))

(defun get-attr-from-kb (kb attr)
  (assoc attr kb))

(defun get-all-attr-values (user attr)
  (if (user-p user)
      (remove-if-not #'(lambda (x) (eq (car x) attr))
		     (user-local-kb user))))

;; STARTING A NEW DAY
;; currently invoked at system restart until we get real alarms

(defun newday (id)
  (let ((user (lookup-user id)))
    (if (and (user-p user)  (eq (user-role user) 'patient))
      (progn 
	(set-alarms user)
	(send-msg `(REQUEST :content (SET-ALARM :hour 23 :minute 55
						:msg (end-of-day  :user ,(user-channel-id user) 
								  ))))
	)
      (da-warn "Unknown user: ~S" id))))

(defun set-alarms (user)
  (let ((id (user-channel-id user)))
    (mapcar #'(lambda (a) (set-alarm id a user))
	    (user-alarms user))
    ))

(defun set-alarm (user-id a user)
  (let ((time (alarm-time a))
	(alarm-msg `(ALARM :user ,user-id
			   :test ,(alarm-test a)
			   :args ,(alarm-args a)
			   :start-state ,(alarm-start-state a))))
    ;; only set alarm if its in the future
    (if *using-alarms* 
	(if (t-before (get-time-of-day) time)
	    (send-msg `(REQUEST :content (SET-ALARM :hour ,(car time) :minute ,(cadr time)
						    :msg ,alarm-msg)))))))
	    ;; otherwise save it as pending onland system will try it when it has a chance
	   #|| (progn
	      (save-pending-alarm user alarm-msg)
	      (trace-msg 3 "~%Alarm at ~S saved to pending because time is past: ~S:" time alarm-msg))))))||#


;; reworked ALARM handler for the new FST system, not backwards compatablie with ASMA!
(defun alarm-handler (&key msg)
  (let* ((id (find-arg-in-act msg :user))
	 (user (lookup-user id))
	 )
    (trace-msg 3 "Alarm received: ~S user KB is ~S" msg (if (user-p user) (user-local-kb user)))
    (if (and (user-p user) 
	     (eq (car msg) 'alarm) 
	     (eq (car (find-arg-in-act msg :msg)) 'idle-check)
	     )

	;; special handling of idle-checks
	(progn
	  (trace-msg 3 "checking idle time: last interaction with user: ~S. last interaction with BA: ~S time of day ~S"  (user-time-of-last-user-interaction user) (user-time-of-last-BA-interaction user) (get-time-of-day))
	  (when (>= (time-difference-in-seconds (user-time-of-last-user-interaction user) (get-time-of-day))
			 (- *max-wait-until-prompt-in-seconds* 1))
	    ;; eventually we might want to save the current state and reestablish it after the alarm
	   
	    (cache-response-for-processing (list (list* 'ALARM 'XX (find-arg-in-act msg :msg))))
	    (invoke-state 'alarm-handler 'push user nil nil nil))
	  )
	
	;; normal case for alarms
	(if (user-p user)
	    (case (car msg)
	      (alarm
	       ;; we only act on the alarm if the system is not currently waiting for an answer from the system
	       ;;  note: if the alarm is persistent, the question will get asked after the other question is completed
	       (if  (or *disabled-wizard*
			(null (user-wizard-response-pending user))` 
			(> (time-difference-in-minutes (user-time-of-last-wizard-interaction user) (get-time-of-day))
			   *max-wait-for-wizard-reply*))
		    ;;(null (user-current-dstate user)))
		    ;; everything is OK to handle the alarm
		    (progn
		      (cache-response-for-processing (list (list* 'ALARM 'XX (find-arg-in-act msg :msg))))
		      (invoke-state 'alarm-handler 'push user nil nil))
		    ;; we are not ready for the alarm right now because there's an ongoing wizard interaction, save it for later
		    (save-pending-alarm user msg)))
	      
	      (end-of-day   ;; here we send an email report if we haven't done so already
	       (trace-msg 3 "END OF DAY check: KB is  ~S." (user-local-kb user))
	       (if (evaltrigger '(null (attr email-sent)) (user-channel-id user) user nil)
		   (send-email-summary (user-channel-id user) user)))
	      )
	    (da-warn "Unknown user found in alarm ~S" msg))
	)))

(defun set-user-alarm-last-asked (user msg)
  "we need to find the alarm structure in the USER object that corresponds to this alarm and update its last-asked field"
  (let* ((start-state (find-arg-in-act msg :start-state))
	 (args (find-arg-in-act msg :args))
	 (alarm (find-if #'(lambda (a) (and (equal (alarm-start-state a) start-state)
					    (equal (alarm-args a) args)))
			 (user-alarms user))))
    (if (alarm-p alarm)
	(setf (alarm-last-asked alarm) (get-time-of-day))
	(da-warn "in set-user-alarm-last-asked: Predefined alarm not found ~S for user ~S" msg (user-channel-id user))
	)))

(defun set-idle-prompt-alarm (user)
  (when (and *using-alarms* (user-p user))
    (send-msg `(REQUEST :content (SET-ALARM :delay ,*max-wait-until-prompt-in-hours*
					  :msg (ALARM :user ,(user-channel-id user) :msg (IDLE-CHECK)))))))

(defun end-of-turn (&key uttnum words channel)
  "if we get here then we were not able to interpret any of the user's utterance"
  (let* ((user (lookup-user channel)))
    (trace-msg 1 "~%Processing END-OF-TURN message ...")
    (if (not *interpretable-utt*)
	(uninterpretable-utterance-handler nil nil nil (if (user-p user) (user-channel-id user) channel)
					   words nil nil user uttnum)
      )
    (setq *interpretable-utt* nil)  ; reset for next turn
    (release-pending-speech-act)
    ))

;;  HERE WE START THE CODE FOR PROCESSING INCOMING MESSAGES
;;    they are either new speech acts or an utterance failed messages

;; UTT-FAILURE handles the case where the IM fails to interpret the input
;;     we just ignore this and see if anothe fragment follows that is interpretable
;;     end-of-turn message handles the failure/wizard interactions
(defun utt-failure (&key channel words code uttnum)
  nil)
  #||(let* ((user (lookup-user channel)))
    (if user
	(when (not (user-success-this-turn user))
	  ;; perform failure code specified in state or use default mechanism
	  (if (and (user-current-dstate user) (state-failure-actions (user-current-dstate user)))
	      (execute-action (state-failure-actions (user-current-dstate user)) channel user uttnum)
	      (uninterpretable-utterance-handler nil nil nil channel words nil nil user uttnum))) ;; otherwise ignore if we already had a success

	(send-reply "Hi. I do not recognize you as a registered participant" channel))))||#

(defun resume-dialogue (&key patient)
  (let ((user (lookup-user patient)))
    (when user
      (trace-msg 3 "Wizard releasing dialogue for patient ~S" patient)
      (setf (user-wizard-controlling-dialogue user) nil)
      (go-to-restart-state user)
      (check-posted-alarms user (user-channel-id user)))))

(defun new-cps-act (&key hyps context channel words uttnum)
  "Takes a set of hypotheses from the next utterance and matches then against the current discourse state"
  (let* ((user (lookup-user channel))
	 (newchannel (if (user-p user) (user-channel-id user) channel)))  ;; convert to internal channel name ('DESKTOP -> "desktop")

    (record-for-current-user '(RECORD WAITING NO) user)
    (setq *current-user* user)
    (trace-msg 3 "~%new act ~S, user is ~S, state is ~S, channel ~S." words 
	       (if (user-p user) (user-name user) newchannel)
	       (if (user-p user) (user-current-dstate user))
	       newchannel)
    (set-idle-prompt-alarm user)
    
    (if user	
	(cond ((user-wizard-controlling-dialogue user) 
	       (trace-msg 3 "System ignoring input ~S, being handled by wizard" words)
	       (clear-pending-speech-acts))
	      ;; if we haven't heard from the user in over some specified time in *delay-to-clear*, and we are in a state wainting for an answer, we bail out
	      ((and 
		(> (time-difference-in-minutes (user-time-of-last-user-interaction user) (get-time-of-day)) *delay-to-clear*)
		(current-dstate user))
	       (update-current-dstate nil nil user nil)
	       (send-reply "Sorry, Its been a while so I forgot what we were doing" newchannel)
	       (check-posted-alarms user newchannel))
	      
	      (t
	       (trace-msg 4 "System processing input ~S" words)
	       (record-transcript user 'user-says words)
	       (let* ((hyp (if (consp hyps) (car hyps)))   ;; just do first one for now
		      (lfs (cons hyp (mapcar #'make-mods-unique (im::remove-unused-context hyp context))))
		      (dstate (current-dstate user)))
		 (setq *words* words)
		 (setq *most-recent-lfs* lfs)
		 (setf (user-time-of-last-user-interaction user) (get-time-of-day))
		 (if dstate
		     (when (not (process-lf-in-state lfs hyps context newchannel words user uttnum))
		       (if (state-implicit-confirm dstate)
			   ;; clear the state and start again
			   (progn
			     (if (consp (state-implicit-confirm dstate))
				 (mapcar #'execute-action (state-implicit-confirm dstate)))
			     (trace-msg 2 "~%Popping state ~S to try triggers"
					(state-id (current-dstate user)))
			     (go-to-restart-state user)
			     (check-for-triggers lfs hyps context newchannel words user *segments* uttnum))
			   ;; we failed to interpret utterance, release send part of input and try again
			   (release-pending-speech-act))
		       )
			     ;;(uninterpretable-utterance-handler lfs hyps context channel words (current-dstate user) nil user uttnum)))
		     (check-for-triggers lfs hyps context newchannel words user *segments* uttnum)))))
	(send-reply "Hi. You are not yet registered for our study" newchannel))))

(defun handle-input-message (msg)
  "this handles spontaneous report from the BA or other components"
  (let* ((msg-content (find-arg-in-act msg :content))  ;; this should be a REPORT
	 (content (find-arg-in-act msg-content :content))
	 (user (lookup-user 'desktop))
	 (dstate (current-dstate user))
	 (newchannel (if (user-p user) (user-channel-id user) "desktop"))
	 (uttnum 'x)
	 (context)
	 (lfs (list content))
	 (hyps))
	     
      (if dstate
      (when (not (process-lf-in-state lfs hyps context newchannel nil user uttnum))
	(if (state-implicit-confirm dstate)
	    ;; clear the state and start again
	    (progn
	      (if (consp (state-implicit-confirm dstate))
		  (mapcar #'execute-action (state-implicit-confirm dstate)))
	      (trace-msg 2 "~%Popping state ~S to try triggers"
			 (state-id (current-dstate user)))
	      (go-to-restart-state user)
	      (check-for-triggers lfs hyps context newchannel nil user *segments* uttnum))
	    ;; we failed to interpret utterance, release send part of input and try again
	    (release-pending-speech-act))
	)
      ;;(uninterpretable-utterance-handler lfs hyps context channel words (current-dstate user) nil user uttnum)))
      (check-for-triggers lfs hyps context newchannel nil user *segments* uttnum)))
  )

(defun make-mods-unique (lf)
  "If an LF has multiple MOD features, we add digits to the second and thrid, etc"
  (if (member :mod lf)
      (multiple-value-bind (pre post)
	  (find-prefix-to-first-mod nil lf)
	(append pre (add-digits 1 post)))
      lf))

(defun find-prefix-to-first-mod (pre lf)
  (when lf
    (if (eq (car lf) :mod)
	(values (append pre (list (car lf) (cadr lf)))
		(cddr lf))
	(find-prefix-to-first-mod (append pre (list (car lf))) (cdr lf)))))

(defun add-digits (n lf)
  (when lf
    (if (eq (car lf) :MOD)
	(cons (add-dig n :mod)
	      (add-digits (+ n 1) (cdr lf)))
	(cons (car lf) (add-digits n (cdr lf))))))

(defun add-dig (n term)
  (intern (format nil "~A~A" term n) util::*keyword-package*))

(defun BA-message-handler (msg)
  (process-lf-in-state (list (find-arg-in-act msg :content)) nil nil 'ba nil (or *current-user* (lookup-user 'desktop))
		       nil))
  
(defun process-lf-in-state (lfs hyps context channel words user uttnum)
  (declare (ignore hyps context words))
  "processes the LF with the patterns associated with the current state. 
      Returns T if interpretation succeeded"
  (when (not (user-p user))
    (format t "~%Warning: process-lf-in-state  called with a bad user argument: ~S" user)
    (setq user *current-user*))
  (let ((s (current-dstate user)))
    (trace-msg 2 "~%Interpreting LF ~S for ~S" lfs (user-name user))
    (trace-msg 1 "~% in state ~S"  (if (state-p s) (state-id s)))
    (trace-msg 4 "~%~S" s)
    (when s
      ;; first extract out terms if desired
      (let ((terms (when *term-extraction-rules*
		     (mapcar #'car
			     (mapcar #'im::match-result-output (interpret-lf lfs *term-extraction-rules*))))))
	(if *term-extraction-rules*
	    (trace-msg 2 "~%Terms extracted are ~S" terms))
	;; next  check for preprocessing rules
	(when (state-preprocessing-ids s)
	  (let ((newlfs (preprocess lfs (state-preprocessing-ids s))))
	    (when newlfs
	      (trace-msg 2 "Preprocessing converted the LFS to ~S" newlfs)
	      (setq lfs newlfs))))
	
	;; interpreting the (possibly preprocessed) form
	(trace-msg 2 "Checking transition arcs for state  ~S" (state-id s))
      	(multiple-value-bind
	      (action next)
	    (find-first-transition-match lfs (state-transitions s))
	  
	  (when action
	    ;; if the user says something interpretable while we're waiting for the wizard, we cancel the wizard request
	    (when (user-wizard-response-pending user) 
	      (setf (user-wizard-response-pending user) nil)
	      (send-msg `(REQUEST :content (WIZARDCANCEL :userchannel ,channel))))
	    (mapcar #'(lambda (a) (execute-action a channel user uttnum)) 
		    (mapcar #'(lambda (x) (add-terms-to-act-if-necessary x terms))
			    (im::match-result-output (car action))
			    ))
	    (record-success user)
	    (invoke-state next nil user (user-dstate-args user) uttnum)  ;; pass along args 
	    (when *last-internal-response*
	      (interpret-new-responses hyps context channel words user uttnum)
	      )
	    T)))
      )))

(defun add-terms-to-act-if-necessary (act terms)
  (if (eq (car act) 'call)
      (list 'call (append (cadr act) (list :terms terms)))
      act))

(defun check-for-triggers (lfs hyps context channel words user segments uttnum)
  "check available triggers"
  (let* ((terms (when *term-extraction-rules*
		 (mapcar #'car
			 (mapcar #'im::match-result-output (interpret-lf lfs *term-extraction-rules*)))))
	 (x (if *term-extraction-rules*
		(trace-msg 2 "~%Terms extracted are ~S" terms)))
	 (possible-triggers (sort (check-segment-triggers lfs segments channel user uttnum) #'> 
				  :key #'(lambda (x) (list-length (im::match-result-cover (car x)))))))
      (declare (ignore x))
    ;; If we have possible triggers, we check each one unless we find an invoked state that succeeds
    (if possible-triggers
	(progn
	  (trace-msg 3 "~%Possible triggers are: ~S~%Checking them one by one until success" possible-triggers)
	  (Try-triggered-states-until-success (caar possible-triggers) (cadar possible-triggers) (cdr possible-triggers) terms lfs hyps context channel words user uttnum))
	;; no applicable trigger: see if there is any more input
	(release-pending-speech-act) 
      ;;(uninterpretable-utterance-handler lfs hyps context channel words (current-dstate user) nil user uttnum)
    )
    ))

(defun try-triggered-states-until-success (result segment backup-results terms lfs hyps context channel words user uttnum)
  (if result
      (progn
	;; we found a segment that matched - first execute the remaining actions
	(mapcar #'(lambda (x) (execute-action x channel user uttnum))
		(mapcar #'(lambda (x) (add-terms-to-act-if-necessary x terms))
			(im::match-result-output result)))
	;; now do state transition, or 
	;;(if (segment-start-state segment)
	;; report status
	(send-status `(OK :in-segment ,(segment-id segment)))
	(update-current-dstate (segment-start-state segment) nil user nil t)
	
	(cond 
	  ;;  the rule involved calling the BA, so we now process the reply
	  (*last-internal-response*
	   (interpret-new-responses hyps context channel words user uttnum)
	   )
   	; If we're now in the end state, we are done!
	  ((or (null (segment-start-state segment)) (eq (segment-start-state segment) 'segmentend))
	   (setq *interpretable-utt* t)   
	   (check-posted-alarms user channel))
	  ;;  the segment triggered a rule, and has a valid destination, so we invoke the destination state
	  ((segment-derived-from-rule segment)
	   (invoke-state (segment-start-state segment) nil user nil uttnum)
	   )
	  (t
	   (when (not (process-lf-in-state lfs hyps context channel words user uttnum))
	     ;; interpretation failed
	     ;;(uninterpretable-utterance-handler lfs hyps context channel words (current-dstate user) segment user uttnum)))
	     (update-current-dstate nil nil user nil t)  ;; clear the failed start-state
	     (trace-msg 3 "Trigger test failed, trying the next ...")
	     (try-triggered-states-until-success (caar backup-results) (cadar backup-results) (cdr backup-results) terms lfs hyps context channel words user uttnum)
	     ))
	  ))
      ;; We tried them all, so checkfor more input
      (release-pending-speech-act) 
      ;;11(uninterpretable-utterance-handler lfs hyps context channel words (current-dstate user) segment user uttnum))
 ))

(defun interpret-new-responses (hyps context channel words user uttnum)
  (when *last-internal-response*
    (let ((resp (pop-response-cache)))
      (trace-msg 3 "~% Processing internal response: ~S" resp)
      (process-lf-in-state resp hyps context channel words user uttnum))))
      

(defun interpret-LF (lfs codes)
  "this calls the im function and remembers the result"
  (remember-result (im::interpret-lf lfs codes)))

(defun check-segment-triggers (lfs segments channel user uttnum)
  "This iterates over all the possible segment triggers and returns a list of (result segment) for the ones that succeed"
  (when segments
    
    (let* ((firstsegment (cdar segments))) ;; segments is an Assoc list
      (trace-msg 3 "Trigger test for segment ~S: ~S" (segment-id firstsegment) (segment-trigger firstsegment))
      (let* ((newlfs (if (segment-preprocessing-ids firstsegment)
			(preprocess lfs (segment-preprocessing-ids firstsegment))
			lfs))
	    (results (interpret-LF newlfs (list (segment-trigger firstsegment))))
	    (result (car results)))
      (if (and (im::match-result-p result)
	       (or (segment-derived-from-rule firstsegment)  ;; if segement was deived from a rule, there is no explicit test
		   (evaltrigger (car (im::match-result-output result)) channel user uttnum))) ;; first result of a trigger is the test
	  (progn
	    (trace-msg 2 "trigger test succeeded: ~S ~S looking for more ...." result firstsegment)
	    (cons (list result firstsegment) (check-segment-triggers lfs (cdr segments) channel user uttnum)))
	  (progn
	    (trace-msg 4 "trigger test failed. looking for more ...")
	    (check-segment-triggers lfs (cdr segments) channel user uttnum)))))))

(defun evaltrigger (test channel user uttnum)
  "A trigger succeeds if the first test succeeds - and if it does, the remaining actions are executed as well"
  (trace-msg 3 "~%evaluating trigger ~S:" test)
  (let ((res (execute-action test channel user uttnum)))
    (trace-msg 3 "Trigger test  ~S" (if res 'succeeded 'failed))
    res))
  ;;(if res
  ;;(mapcar #'(lambda (x) (execute-action x user)) (cdr tests)))))
  
(defun preprocess (lfs rulegroup-ids)
  "Find first preprocessing rule in the RULEGROUP that matches the LFS"
  (trace-msg 3 "Preprocessing with rules ~S" rulegroup-ids)
  (let ((ans (interpret-lf lfs rulegroup-ids)))
    (if ans
	(progn
	  (trace-msg 2 "~%Matched preprocessing rule: top hit is ~S" (im::match-result-rule-id (car ans)))
	  (trace-msg 3 "~%from rule ~S" (im::match-result-output (car ans)))
	  (trace-msg 4 "~%   Other rules` are ~S" (cdr ans))
	  (im::match-result-output (car ans)))
	(progn 
	  (trace-msg 3 "~%None matched: preprocessing finished")
	  nil))))


(defun find-first-transition-match (lfs transitions)
  (when transitions
    (let* ((im::*trace-level* dagent::*trace-level*)  ;; turn on IM tracing the the rule matching
	   (ans (interpret-lf lfs (list (transition-pattern (car transitions))))))
	  
      (if ans (progn (trace-msg 2 "~%Matched rules: top hit is ~S" (im::match-result-rule-id (car ans)))
		     (trace-msg 3 ": ~%~S" (car ans))
		     (trace-msg 4 "~%   Other rules are ~S" (cdr ans)))
	  )
      (if ans
	  (values ans (transition-destination (car transitions)))
	  (find-first-transition-match lfs (cdr transitions))))
    ))

(defun clear-pending-speech-acts (uttnum channel)
  "we send a msg to the IM to clear any pending speech acts"
  (send-msg '(request :content (clear-speech-acts))))

(defun release-pending-speech-act nil
  "we send a msg to the IM to clear any pending speech acts"
  (send-msg '(request :content (release-pending-speech-act))))

(defun execute-action (act channel user uttnum)
  (when act
    (trace-msg 1 "Executing ~S ..." (car act))
    (trace-msg 2 "~%~S" act)
    (setq act (instantiate-dstate-args act user))
    (let ((ans (case (car act)
		 (perform (mapcar #'(lambda (x) (execute-action x channel user uttnum))
				  (cdr act)))
		 (and (every #'(lambda (x) (execute-action x channel user uttnum))
			     (cdr act)))

		 ;; list of control meds
		 (druglist
		  (mapcar #'(lambda (b) (cadr (alarm-args b))) 
			  (remove-if-not #'(lambda (a) (equal (alarm-start-state a) 'CONTROL-MED1))
				       (user-alarms user))))

		 ;; wait and check on user again after suggesting inhaler use
	       (set-inhaler-alarm
		(send-msg `(REQUEST :content (SET-ALARM :delay ,*wait-after-inhaler*
		    :msg (ALARM :user ,(user-channel-id user) :test (nop) :args nil :start-state MS3-1))))
		)
	       (set-inhaler-alarm-2
		(send-msg `(REQUEST :content (SET-ALARM :delay ,*wait-after-inhaler*
		    :msg (ALARM :user ,(user-channel-id user) :test (nop) :args nil :start-state MS3-2))))
		)

	       (call-wizard-inhaler-failed
		(call-wizard nil nil nil channel nil nil nil user uttnum))

	       (record
		(record-for-current-user act user))
	       (say
		(clear-pending-speech-acts  uttnum channel)
		(setf (user-time-of-last-user-interaction user) (get-time-of-day))
		(record-transcript user 'system-says act)
		(prepare-reply (find-arg-in-act act :content) channel))
	       (say-one-of 
		(clear-pending-speech-acts  uttnum channel)
		(setf (user-time-of-last-user-interaction user) (get-time-of-day))
		(record-transcript user 'system-says act)
		(prepare-reply (pick-one (find-arg-in-act act :content)) channel))
	       (compound-say 
		(clear-pending-speech-acts  uttnum channel)
		(setf (user-time-of-last-user-interaction user) (get-time-of-day))
		(record-transcript user 'system-says act)
		(prepare-reply (compound-say (find-arg-in-act act :content) (user-dstate-args user) user) channel))
	       (say-one-of-next
		(clear-pending-speech-acts  uttnum channel)
		(note-speech (pick-one (find-arg-in-act act :content))))
	       (null
		(null (execute-action (cadr act) channel user uttnum)))
	       (attr-contains
		(remove-if-not #'(lambda (x) 
			  (element-in (caddr act) x))
		      (get-all-attr-values user (cadr act))))
	       (attr 
		(get-attr user (cadr act)))
	       (only-one
		(if (consp (cadr act))
		    (= (list-length (execute-action (cadr act) channel user uttnum)) 1)
		    (= (list-length (get-all-attr-values user (cadr act))) 1)))
	       (only-two
		(= (list-length (get-all-attr-values user (cadr act))) 2))
	       (duration-since
		(and (numberp (third act))
		     (> (time-difference-in-minutes (get-attr user (second act)) (get-time-of-day)) (third act))))
	       (words *words*)
	       (send-msg 
		(send-msg (install-variables user (cdr act))))
	       (call
		(apply (car (cadr act)) (list (cdr (cadr act)) channel user uttnum)))

	       ;;  here are the CPS/BA builtin actions
	       (update-csm
		(update-csm (cadr act)))
	       (query-CSM
		(query-csm :content (find-arg-in-act act :content)))
	       (invoke-ba 
		(invoke-ba (cdr act) user channel uttnum)
		nil)
	       ((notify-ba request)
		(notify (cdr act) user channel uttnum))
	       (set-alarm
		(if *using-alarms*
		  (let ((delay (find-arg-in-act act :delay))
			(msg (find-arg-in-act act :msg)))
		    (notify (list :msg (list 'SET-ALARM :delay delay :msg (list 'ALARM :user (user-channel-id user) :msg msg))
				  :msg-type 'REQUEST)
			    user channel uttnum))
		  (trace-msg 1 "Not using alarms!")))
	       (extract-goal-description
		(apply #'extract-goal-description (cdr act)))
	       ;; this function is called only if it is the the action slot of a state, so results are cached so the pattern rules 
	       ;;  can access them
	       (take-initiative?
		(let* ((result (apply #'take-initiative? (instantiate-dstate-args (cdr act) user)))
		       ;;(result-value (or (find-arg-in-act result :result) 'NO))
		      )
		  (cache-response-for-processing (list result))))
	       (any-pending-speech-acts?
		(let* ((result (apply #'any-pending-speech-acts? (instantiate-dstate-args (cdr act) user)))
		       )
		  (cache-response-for-processing (list result))))
	       (set-cps-variable 
		(set-CPS-variable (cadr act) (caddr act)))
	       (generate
		(invoke-generator (cdr act) user channel uttnum))
	       ;; these are domain psecific to ASMA and should be recoded to use the CALL action
	       (no-contact-today-yet (no-contact-today-yet user))
	       (email-summary 
		(send-email-summary channel user))
	       (nop t)   ; don't do anything (fortriggers)
	       (continue
		(cache-response-for-processing '((continue :arg dummy))))
	       (clear-pending-speech-acts
		(clear-pending-speech-acts  uttnum channel)
		(setq *interpretable-utt* nil)		
		)
	       (true (clear-pending-speech-acts  uttnum channel))
	       (next (release-pending-speech-act))
	       (otherwise
		(format t "~% WARNING: action not known: ~S:" act)))))
    (trace-msg 3 " result of execute-action is ~S" ans)
    ans)))

;;  BA invocation

(defun invoke-BA (args user channel uttnum)
  "here we invoke a backend aganet with a message and record the result for the followup state.
    Since we are using send and wait we don't have syncronization problems"
  ;; clear clear any remaining input in the input stream
;  (clear-pending-speech-acts uttnum channel)
  (setq *interpretable-utt* t)
  (setf (user-time-of-last-BA-interaction user) (get-time-of-day))
  (let* ((msg (instantiate-dstate-args (find-arg args :msg) user))
	 (x (send-status `(WAITING :on ,(car msg))))
	 (reply (send-and-wait `(REQUEST :content ,msg)))
	 (content (find-arg-in-act reply :content))
	 (context (find-arg-in-act reply :context))
	 (converted-response (append (list* 'BA-RESPONSE 'X (car reply) :psact content) (list :content content :context context))))
    (declare (ignore x))
    ;; (send-status '(READY GOT-REPLY)) ; now OK turns the light green
    ;;(converted-response (list 'BA-RESPONSE 'X (car reply) :content content :context context) )) ; probably the same as (list* 'BA-RESPONSE 'X reply) ?
    (setf (user-time-of-last-BA-interaction user) (get-time-of-day))  ;; we set if again now we have a response
    ;;  we record the BA response
    (if content (cache-response-for-processing (list converted-response))
	(format t "~%WARNING: BA returned uninterpretable response: ~S:" reply))
    (trace-msg 3 "~% *last-internal-response* is ~S~%" *last-internal-response*)	  
   ))

(defun notify (args user channel uttnum)
  "here we invoke the BA with a message but don't expect a response"
;  (clear-pending-speech-acts uttnum channel)
  (setq *interpretable-utt* t)
  (let ((msg (instantiate-dstate-args (find-arg args :msg) user))
	(msg-type (instantiate-dstate-args (find-arg args :msg-type) user))
	)
    (send-msg `(,msg-type :content ,msg))
    ))

(defun extract-goal-description (&key cps-act context result goal-id)
  (let* ((lf (find-lf-in-context (find-arg-in-act cps-act :what) context))
	 (id (second lf)))
    (append (im::match-vals nil goal-id id)
	    (im::match-vals nil result (list lf)))))

(defun find-lf-in-context (id context)
  (find-if #'(lambda (x) (eq (second x) id))
		 context))

(defun generate-AKRL-context (&key what result)
  (let ((reduced-context (remove-unused-context-with-root what *most-recent-lfs*)))
    (when reduced-context
      (im::match-vals nil result (mapcar #'im::convert-lf-to-akrl reduced-context)))))

(defun find-attr (&key result feature)
  (im::match-vals nil result (get-attr *current-user* feature)))

(defun find-feature-from-attr  (&key result attr feature)
  (im::match-vals nil result (find-arg-in-act (get-attr *current-user* attr) feature)))

(defun extract-feature-from-act (&key result expr feature)
  (format t "~%extracting features: expr = ~S feature = ~S result = ~S" expr feature (find-arg-in-act expr feature))
  (im::match-vals nil result (find-arg-in-act expr feature)))

(defun replace-feature-val-in-act (&key result act feature newval)
  (format t "~%replacing value: act = ~S feature = ~S value = ~S" act feature newval)
  (im::match-vals nil result (replace-arg-in-act act feature newval)))
  
(defun invoke-generator (msg user channel uttnum)
  "here we invoke the BA with a message and record the result for the followup state.
    Since we are using send and wait we don't have syncronization problems"
  (setf (user-time-of-last-user-interaction user) (get-time-of-day))
  (let ((expanded-msg (expand-args msg)))
    (Format t "~%~%==========================~% System generating: ~S ~%========================~%" expanded-msg)
    ;;  we clear any remaining input as we generate  ;;; we don't because it might just be responding with an "ok" and we want to process the next speech act
;    (clear-pending-speech-acts uttnum channel)  
;    (setq *interpretable-utt* t)
    (send-msg `(REQUEST :content ,(cons 'GENERATE expanded-msg)))))

(defun expand-args (msg)
  "this examines the msg for variables that store the CPS state and replaces them with the values"
  (when msg  
    (if (consp msg) 
	(cond 
	  ((and (eq (car msg) 'v) (keywordp (cadr msg)))
	   (or (lookupCPSvar (cadr msg)) msg))
	  ((and (eq (car msg) 'vv) (keywordp (cadr msg)))
	   (let ((lv (lookupCPSvar (cadr msg))))
	     (or (cadr lv) lv  msg)))
	  (t
	   (cons (expand-args (car msg)) (expand-args (cdr msg)))))
	msg)))

;;   STUFF that mostly specific to ASMA

(defun install-variables (user expr)
  "replaces all expression of form (:lookup X) with the value of X in the local KB"
  (if (consp expr) 
      (if (eq (car expr) :lookup)
	  (get-attr user (cadr expr))
	  (reuse-cons (install-variables user (car expr)) (install-variables user (cdr expr)) expr))
      expr))
	 

(defun element-in (x ll)
  "T if x appears somewhere in ll"
  (when ll
    (or (equal x ll) 
	(if (consp ll)
	    (or (element-in x (car ll)) (element-in  x (cdr ll)))))))

(defun no-contact-today-yet (user)
  (equal (user-time-of-last-user-interaction user) '(0 0)))
			       
(defun note-speech (text)
  "This saves some text that will be used in the next message"
  (setq *saved-text* (concatenate 'string *saved-text* text " ")))

(defun prepare-reply (text channel)
  "This appends text to any saved speech and say it"
  (trace-msg 4 "~% preparing text for output, saved-text is ~S" *saved-text*)
  
  (send-reply (concatenate 'string *saved-text* text) channel)
  (setq *saved-text* ""))

(defun invoke-state (stateID push? user args uttnum &optional input)
  (let ((channel (user-channel-id user)))
    (trace-msg 1 "~S state ~S" (if (eq push? 'push) "Pushing" "Invoking") stateID)
    (record-transcript user 'invoke-state stateID)
    ;; ending a segment
    (if (or (eq stateID 'segmentend) (null stateID))
	(progn
	  (trace-msg 2 "Segment ended")
	  (pop-current-dstate user)
	  (check-posted-alarms user channel)
	  (release-pending-speech-act)
	  )
    ;; shifting to a new state
    (let ((newstate (find-state stateID)))
      (update-current-dstate newstate push? user args)
      
      (if newstate
	  (if (null input)
	      ;; if no input provided, we execute the action to state the state (typically asking a question)
	      (progn
		(execute-action (state-action newstate) channel user uttnum)
		;; executing the action may have resulting in input.
		(if *last-internal-response*
		    (interpret-new-responses nil nil channel nil user uttnum))
		    ;; otherwise we are done, except for the immediate transition states
		(when (state-immediate-transition newstate)
		  (trace-msg 1 "immediate transition to state ~S"  (state-immediate-transition newstate))
		  (invoke-state (state-immediate-transition newstate) nil user nil uttnum)))
	      ;; if input is provided, we process it in the new state
	      (process-lf-in-state input nil nil channel nil user uttnum))
	     
	  (trace-msg 1 "~% Transferring to unknown state: ~S" stateID)))
    )))

      
;;  interactions with the Wizard
;;  this is called when the system fails to understand any part of an utterance
;;  the wizard is called only for dialogue states that are marked as critical using the variable *critical-states-for-wizard*

(defvar *critical-states-for-wizard* nil)

(defun uninterpretable-utterance-handler (lfs hyps context channel words state segment user uttnum)
  "Assuming we're in a specific state, we invoke the wizard"
  ;; sometimes state is not specified, and we get is from the USER record
  (if (and (null state) (user-p user))
      (setq state (current-dstate user)))
  
  (if (or *disabled-wizard*   ;; do not invoke wizard if   (1) wizard is disabled
	  (and state         ;; or we are in a non-critical state and haven'y exceeded the reask limit
	       (and (not (member (state-id state) *critical-states-for-wizard*))
		    (< (user-immediately-prior-turns-that-failed user) *reask-limit*))) 
; commented this out so as to invoke the wizard if the system doesn't understand a user-initiated utterance on the first try
;	  (and (null state) (< (user-immediately-prior-turns-that-failed user) 1))  ;; or (3) its a user initiated utterance and its not a reask
	  )
      (progn
	(when (not *silent-failures*) 
	  (send-reply (pick-one '("Can you rephrase that?" "I didn't understand that. Can you rephrase?" "Sorry, I didn't catch that. Can you rephrase?")) 
		      channel)
	  (update-prior-failures user)
	  (reask-question user channel uttnum))
	(release-pending-speech-act)  ;; why is this here - we should be at the end of the utterance already?
	)
      
      (call-wizard lfs hyps context channel words state segment user uttnum)
      
      ))

(defun call-wizard (lfs hyps context channel words state segment user uttnum)
  (clear-pending-speech-acts uttnum channel)
  (if (null state) (setq state *empty-state*))
  (trace-msg 1 "Wizard ~S: call in state ~S and segment ~S words ~S LF: ~S~%" (user-wizard user) (state-id state) segment words hyps)
  (setf (user-wizard-response-pending user) T)
  (if (and (> (time-difference-in-minutes (user-time-of-last-wizard-interaction user) (get-time-of-day))
	      *wizard-prompt-delay-time*)
	   (wizard-channel user))
      (send-reply (format nil "~A needs attention" (user-name user)) (wizard-channel user)))
  (setf  (user-time-of-last-wizard-interaction user) (get-time-of-day))
  (send-msg-with-continuation `(REQUEST :content (WIZARDQ :id ,(user-wizard user)
							  :words ,words 							  
							  :options ,(mapcar #'transition-description (state-transitions state))
							  :userchannel ,channel))
			      #'(lambda (msg) (handle-wizard-reply msg lfs words channel hyps context state 
								   (if (segment-p segment) (Segment-id segment)) user uttnum)))
  )

(defun reask-question (user channel uttnum)
  (let* ((current-state (current-dstate user))
	 (current-action (if (state-p current-state)
			     (state-action current-state))))
    (if (member (car current-action) '(compound-say say))
	(execute-action current-action channel user uttnum))))
	 

(defun handle-wizard-reply (msg lfs words channel hyps context state segment-id user uttnum)
  ;;(setf (user-wizard-response-pending user) nil)
  (setf (user-immediately-prior-turns-that-failed user) 0)
  (setf (user-wizard-controlling-dialogue user) nil)
  (setf (user-wizard-response-pending user) nil)
  (push (list :wizard (user-wizard user) :state (state-id state) :words words :lfs lfs :channel channel
	      :choice msg) *wizard-responses*)
  (case (car msg)
    (wizard-chose
     (let* ((choice (find-arg-in-act msg :choice))
	    (transition (find-if #'(lambda (x) (string= (transition-description x) choice)) (state-transitions state))))
       (format t "~%Wizard: Choice was ~S" choice transition)
       (if transition 
	   (let ((outp (im::subst-in (transition-output transition) 
				     (apply-transition-tests (transition-tests transition) nil))))
	     (every #'(lambda (x) (execute-action x channel user uttnum)) outp)
	     (invoke-state (transition-destination transition) nil user nil uttnum))
	    (trace-msg 3 "Wizard choice ~S not understood for patient ~S"  choice channel))))
    (uninterpretable
     (send-reply "I didn't understand. Can you rephrase?" channel))
    (wrong-question
     (trace-msg 3 "Wizard indicated wrong question. Trying others ....")
     (try-other-questions lfs words hyps context channel state segment-id user uttnum))
    (ignore
     (trace-msg 3 "Wizard said to ignore the utterance")
     (if (state-triggered user)
	 (update-current-dstate nil nil user nil)))
    (wizard-took-over
     (trace-msg 3 "SHOULD NEVER GET HERE!!! Wizard taking over dialogue")
     (setf (user-wizard-controlling-dialogue user) t)
     (go-to-restart-state user)
     ))
  )
 ;; (trace-msg 4 "~%Checking alarms ...")
 ;; (check-posted-alarms user channel))

(defun wizard-takeover (&key patient)
  (let ((user (lookup-user patient)))
    (trace-msg 3 "Wizard taking over dialogue for patient ~S " (user-name user))
    (setf (user-wizard-controlling-dialogue user) t)
    (setf (user-wizard-response-pending user) nil)
    (go-to-restart-state user))
  )

(defun try-other-questions (lfs words hyps context channel state segment-id user uttnum)
  (trace-msg 3 "~%Trying other segments past ~S" segment-id)
  (update-current-dstate nil nil user nil) 
  (check-for-triggers lfs hyps context channel words user 
		      (if segment-id (remove-segments-up-to segment-id *segments*)
			  *segments*) uttnum))

(defun remove-segments-up-to (seg segments)
  (when segments
    (if (eq (caar segments) seg)
	(progn
	  (trace-msg 3 "~%remaining segements are ~S" (cdr segments))
	  (cdr segments))
	(remove-segments-up-to seg (cdr segments)))))
  
    
(defun send-reply (msg channel)
  (trace-msg 3 "Saying ~S: ~S" channel msg)
  (if (and (or (string-equal channel "desktop") (eq channel 'DESKTOP)) (eq parser::*in-system* :asma))
      (format t "~%**********************~%*~%SYS: ~S~%*~%**************************" msg)
      (send-msg `(REQUEST :content (SAY :content ,msg :channel ,channel)))
  ))

(defun send-status (status)
  (send-msg `(TELL :content (component-status
			     :who DAGENT ;;; FIXME: surely we must know our name, no?
			     :what ,status))))

(defun pick-one (elements)
  (nth (random (list-length elements)) elements))


#||(defun check-posted-alarms (user channel)
  "this checks for alarms that have passed, but the conditions have not been satisfied yet"
  (when user
    (let* ((now (get-time-of-day))
	   (possible-alarms (remove-if-not #'(lambda (a)
					       (trace-msg 4 "~%Checking ~S" a)
					       (let ((diff (time-difference-in-minutes (alarm-time a) now)))
						 (and (alarm-persistent a)
						      (t-before (alarm-time a) now)    
						      (< diff 240) ;; give up after 4 hours
						      (> (time-difference-in-minutes (alarm-last-asked a) now) 60)  ;; don't ask again for half an hour
						      (evaltrigger (alarm-test a) channel user nil))))
					   (user-alarms user))))
      (trace-msg 3 "Checking alarms: found ~S" possible-alarms)
      (when possible-alarms
	(let ((a (first possible-alarms)))
	  (setf (alarm-last-asked a) (get-time-of-day))
	  (invoke-state (alarm-start-state a) nil user (alarm-args a) nil)))
      )))
||#

(defun check-posted-alarms (user channel)
  "This checks for an reinvokes any alarms that arose but were postponed for some reason"
  (when (and user (user-pending-alarms user))
    (alarm-handler :msg (pop (user-pending-alarms user)))))

;;  utility for generating patterns,

(defun lf-pattern (lfs)
  (let* ((bndgs (mapcar #'(lambda (x)
			  (cons (second x) (im::make-var :name (gentemp "X"))))
		       lfs))
	 (newlfs (substitute-in lfs bndgs)))
    
    (mapcar #'(lambda (x) (cons (car x) (remove-args (cdr x) '(:start :end :tma :mods :mod))))
	    newlfs)))

(defun substitute-in (lfs bndgs)
  (if bndgs
    (substitute-in (subst (cdar bndgs) (caar bndgs) lfs) (cdr bndgs))
    lfs))
			  
(defun get-time-of-day ()
  (multiple-value-bind
	(second minute hour)
      (get-decoded-time)
      (declare (ignore second))
    (list hour minute second)))


(defun t-before (time1 time2)
  "compares times in format (hour minute)"
  (or (< (car time1) (car time2))
      (and (= (car time1) (car time2))
	   (< (cadr time1) (cadr time2)))))

#||(defun since-last-interaction (durationlimit user)
  (let* ((duration (time-difference-in-minutes (user-time-of-last-user-interaction user) (get-time-of-day))))
    (> duration durationlimit)))||#

(defun time-difference-in-minutes (t1 t2)
  (+ (* (- (car t2) (car t1)) 60) (- (cadr t2) (cadr t1))))

(defun time-difference-in-seconds (t1 t2)
  (+ (* (- (car t2) (car t1)) 3600) (* (- (cadr t2) (cadr t1)) 60) (- (caddr t2) (caddr t1))))

;; ========================================
;;  Built-in functions supporting the automatic construction of a rules from patterns

;; this function allows defining patterns for a property on a numberic scale
(defun define-rules (type activities property rolename)
  (mapcar #'(lambda (x)
	      (let ((value (car x))
		    (rules (cdr x)))
	      (apply #'add-preprocessing 
		     (list type
			  (mapcar #'(lambda (pat)
				      (rule :pattern (append pat (list (gentemp "x")) (list (list property rolename value)))))
				  rules)))))
	  activities))


(defun compound-say (x args user)
  "X is a list of strings or variable names and defaults, e.g., (\"have you taken your\" (list :name \"controller med\"))"
  (when x
    (if (stringp (car x))
	(concatenate 'string (car x) (compound-say (cdr x) args user))
	(let* ((property (caar x))
	       (default (cadar x))
	       (value (or (find-arg args property) (get-attr user property))))
	  (concatenate 'string (or value default) (compound-say (cdr x) args user))))))

;; Computing coverage of the match
(defun compute-start-end-of-match nil
  (if (and (consp *most-recent-result*) (im::match-result-p (car *most-recent-result*)))
      (let* ((cover (im::match-result-cover (car *most-recent-result*))))
	(im::add-start-and-end-for-result cover *most-recent-lfs*))))

;;;===========TESTING FUNCTIONS======

(defun wiz-take-over (inreplyto)
  (im::send-msg `(tell :sender AsmaWizard :receiver DAgent :content (wizard-took-over :patient "desktop") :in-reply-to ,inreplyto)))

(defun wiz-ignore (inreplyto)
  (im::send-msg `(request :sender AsmaWizard :receiver DAgent :content (ignore :patient "desktop") :in-reply-to ,inreplyto)))

(defun wiz-choose (val inreplyto)
  (im::send-msg `(tell  :receiver dagent :content (wizard-chose :patient "desktop" :choice ,val :in-reply-to ,inreplyto))))

;; doesn't work
(defun wiz-say (msg)
  (im::send-msg `(tell :sender AsmaWizard :content (say :patient "desktop" :message ,msg))))

  
