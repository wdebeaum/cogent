;;  Managing the collaborative probem solving state

(in-package :dummy)

(defvar *last-query-id* nil)  ;; we need this to generate ANSWER messages
(defvar *last-query-what* nil)  ;; we need this to generate ANSWER messages
(defvar *last-query-in-context* nil)  ;; we need this to generate ANSWER messages
(defvar *last-active-goal*)
(defvar *id-to-what-table* (make-hash-table))
(defvar *id-to-act-table* (make-hash-table))
(defun process-reply (msg args result)
  (let ((reply-with (find-arg-in-act msg :reply-with))
	(sender (find-arg-in-act msg :sender)))
    ;;(format t "~%========================================~% DUMMY: Received: ~S ~% Sending ~S~%==========================~%" msg result)
    (if reply-with
	(send-msg (append (list 'reply :receiver sender :content result) (list :in-reply-to reply-with)))
	(send-msg (list 'tell :content result)))))


(defun process-evaluate (msg args)
  (let* ((reply-with (find-arg-in-act msg :reply-with))
	 (sender (find-arg-in-act msg :sender))
	 (content (find-arg args :content))
	 (context (find-arg args :context))
	 (result 
	  (case (car content)
	    ((adopt assertion select abandon)
	     (let* ((head (find-lf-in-context-tmp context (find-arg-in-act content :what)))
                    (id (find-arg-in-act content :id))
		    (headtype (find-arg head :instance-of))
		    (content-as (find-arg-in-act content :as)))
	       (format t "~% Evaluating ~S with headtype ~S" content headtype)
	       (setf (gethash id *id-to-what-table*) head)
	       (setf (gethash id *id-to-act-table*) content)
	       ;(format t "~% content-as ~S" content-as)
	       (case headtype
		 (ont::cause-effect
		  (let* ((prop (find-lf-in-context context (find-arg head :formal)))
			 (color (find-arg prop :instance-of)))
		    (if (eq color 'ONT::GREEN)
			(list 'report :content (list 'failure :type 'CANNOT-PERFORM :what (second head)
						     :reason '(not enough green blocks SIFT to provide the details) :context context))
			(list 'report :content (list 'acceptable :what content :context context)))))
		 ;;(list 'report :content (list 'failure :type 'FAILED-TO-INTERPRET :what content :reason '(SOME-REASON) :context context)
		 ((ont::create ont::execute)  ;; this is "let's make a N block tower" ;; ont::execute is for "you do it"
		  (let* ((obj (find-lf-in-context context (find-arg head :affected-result)))
			 (mod (find-lf-in-context context (find-arg obj :mod)))
			 (ground (find-lf-in-context context (find-arg mod :ground)))
			 (size (find-arg ground :amount)))
		    (format t "~% size= ~S ground=~S mod = ~S" size ground mod)
		    (if (and (numberp size) (> size 4))
			(list 'report :content (list 'unacceptable :type 'CANNOT-PERFORM :what content ;(second head)
						     :reason '(not enough blocks SIFT to provide the details)) :context context)			 
		      (case (car content-as)
			(elaboration
			 (list 'report :content (list 'acceptable
						      :what content
						      :effect (list 'ADOPT :id 'M3 :what 'P1 
								    :as (substitute 'modification 'elaboration content-as))
						      )
			       :context (append context '((ONT::RELN P1 :instance-of the-modified-event))))
			 )
			(modification
			 (list 'report :content (list 'acceptable
						      :what content
						      :effect  `(ADOPT :id M1 :what P1 :as ,content-as))
			       :context (append context '((ONT::RELN P1 :instance-of the-modified-event)))
			       )
			 )		     
			(otherwise
			 (list 'report :content (list 'acceptable :what content) :context context)
			 )
			)
		  
		      )))
		 	
		 
		
		 (otherwise
		  (list 'report :content (list 'acceptable :what content) :context context)
		  ;(list 'report :content (list 'unacceptable :type 'NO-GOOD :what content) :context context)
		   )
	       )))
	    ((ask-wh ask-if)
	     (setq *last-query-id* (util::find-arg-in-act content :query))
	     (setq *last-query-what* (util::find-arg-in-act content :what))
	     (setq *last-query-in-context* (util::find-arg-in-act (util::find-arg-in-act content :as) :goal))
	     (setf (gethash (find-arg-in-act content :id) *id-to-what-table*) *last-query-what*)
	     (setf (gethash (find-arg-in-act content :id) *id-to-act-table*) content)
	     (list 'report :content (list 'acceptable :what content) :context context)
	     )
	    (answer ;; currently we always like answers
	     (list 'report :content (list 'acceptable :what content
					  :effect `(ADOPT :ID m2 :what P1 :as (MODIFICATION :of ,*last-active-goal*)))
		   
		   :context (append context '((ONT::RELN P1 :instance-of the-modified-event))))
	     )
	    )
	   ))
	 
		  
    ;;(format t "~%========================================~% DUMMY: Received: ~S ~% Sending ~S~%==========================~%" msg result)
    (if reply-with
	(send-msg (append (list 'reply :receiver sender :content result) (list :in-reply-to reply-with)))
	(send-msg (list 'tell :content result)))))
  
(defun reply-to-message (msg result)
  (let ((reply-with (find-arg-in-act msg :reply-with))
	(sender (find-arg-in-act msg :sender)))
    (send-msg (append (list 'reply :receiver sender :content result) (list :in-reply-to reply-with)))))

(defun what-next (msg args)
  (let ((active-goal (find-arg args :active-goal))
	(context (find-arg args :context)))
    (format t "~%DUMMY: WHAT-NEXT active goal is ~S" active-goal )
    (let* 
	(;(target (find-arg-in-act active-goal :what))
	 (target active-goal)
         (target-what (gethash target *id-to-what-table*))
	 (act (find-lf-in-context-tmp context target-what))
	 (condition-id (find-arg act :content))
	 (condition (find-lf-in-context context condition-id))
	 (action (find-arg condition :action))
	 (result (find-arg condition :result))
	 )
      (format t "~%condition is ~S" condition)
      (format t "~%content is ~S" (gethash target *id-to-act-table*))
      (if (null
            (case (first (gethash target *id-to-act-table*))
	      (assertion
	       (reply-to-message msg
	
			   `(REPORT :content (EXECUTION-STATUS :goal ,target :status ONT::DONE)
				    :context ,context)
						   
	  		   ))
	      ((ask-if ask-wh) ;; This should be the up-to-date answer (but still dumb)
	       (reply-to-message msg
	
				 `(REPORT :content (ANSWER :value ONT::FALSE :what ,(gethash target *id-to-what-table*)
							   :to ,target :justification BA-QUERY-111)
				    :context ((ONT::RELN BA-QUERY-111 :INSTANCE-OF ONT::SIMULATION :QUERY (SATISFIES-PATTERN p1) :ANSWER (SUCCESS :content (:satisfies-rate 0.0 :num-sim 10))))
						    )
						   
			   ))
              (otherwise NIL) 
            ))
      (case (find-arg act :instance-of)
	(ont::evaluate
	 (reply-to-message msg
	
			   `(REPORT :content (SOLUTION :what E2  :goal ,target :justification R02)
				    :context ,(cons `(RELN E2 :INSTANCE-OF ONT::INCREASE :AGENT ,action :RESULT ,result)
						    context))
						   
			   ))

	(ont::identify 
	 (let ((obj (find-lf-in-context context (or (find-arg act :affected) (find-arg act :neutral)))))
	   (case (find-arg obj :instance-of)
	     (ont::protein
	      (reply-to-message msg
				`(REPORT :content (SOLUTION :what E1 :goal ,target :justification R01)
					 :context ((RELN E1 :INSTANCE-OF ONT::EQUALS :NEUTRAL S1 :NEUTRAL1 PK01)
						   (A PK01 :INSTANCE-OF ONT::PROTEIN :NAME KRAS :DBID P01116)
						   (RELN R01 :INSTANCE-OF ONT::HAVE :NEUTRAL Q1 :NEUTRAL1 MUT1)
						   (QUANT Q1 :INSTANCE-OF ONT::PATIENT :QUAN N2 :DOMAIN S2)
						   (A N2 :INSTANCE-OF ONT::PERCENTAGE :VALUE 88)
						   (A S2 :INSTANCE-OF ONT::SET :ELEMENT-TYPE P2)
						   (KIND P2 :INSTANCE-OF ONT::PATIENT :MODS (R2))
						   (RELN R2 :INSTANCE-OF ONT::HAVE :NEUTRAL P2 :NEUTRAL1 C01)
						   (A C01 :INSTANCE-OF ONT::PANCREATIC-CANCER)
						   (A MUT1 :INSTANCE-OF ONT::MUTATION :MODS (RM1 RM2))
						   (RELN RM1 :INSTANCE-OF ONT::HAVE :NEUTRAL PK01 :NEUTRAL1 MUT1)
						   (RELN RM2 :INSTANCE-OF ONT::CAUSE-EFFECT :AGENT RM1 :RESULT RA1)
						   (RELN RA1 :INSTANCE-OF ONT::ACTIVATE :AGENT MUT1 :AFFECTED PK01))))
	      )

	     (ont::medication
	      (reply-to-message msg
				`(REPORT 
				  :content (FAILURE :what F1 :as (SUBGOAL :of ,target))
				  :context ,(cons `(A F1 :instance-of ONT::LOOK-UP :neutral ,(second obj))
						  context))))
	     (otherwise 
	      (reply-to-message msg
				`(REPORT :content (WAIT)))))))

	(ont::query-model
	 (reply-to-message msg
	
			   `(REPORT :content (ANSWER :what ONT::FALSE  :goal ,target :justification BA-QUERY-111)
				    :context (ONT::RELN BA-QUERY-111 :INSTANCE-OF ONT::SIMULATION :QUERY (SATISFIES-PATTERN p1) :ANSWER (SUCCESS :content (:satisfies-rate 0.0 :num-sim 10)))
						    )
						   
			   ))
	
	(otherwise 
	       (reply-to-message msg
				 `(REPORT :content (WAIT))))))
  
	  
	)))

       
      
(defun find-lf-in-context (context id)
  (if (consp context) 
      (find id context :key #'cadr)
    nil)
  )

(defun find-lf-in-context-tmp (context id)  ; id not used
  (if (consp context) 
      (find-if #'(lambda (x) (member (find-arg x :instance-of)
				 '(ONT::CREATE ONT::PUT-B6-ON-THE-TABLE ONT::Please-put-B7-on-B6 ONT::PUT
				   ONT::EXECUTE)))
	       context)
    nil)
  )

(defun restart-dummy nil
  (setq *replyCounter* 0))

(defun send-initial-messages nil
     nil)

