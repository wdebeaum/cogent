;;  Managing the collaborative probem solving state

(in-package :dummy)

(defun process-reply (msg args result)
  (let ((reply-with (find-arg-in-act msg :reply-with))
	(sender (find-arg-in-act msg :sender)))
    ;;(format t "~%========================================~% DUMMY: Received: ~S ~% Sending ~S~%==========================~%" msg result)
    (if reply-with
	(send-msg (append (list 'tell :receiver sender :content result) (list :in-reply-to reply-with)))
	(send-msg (list 'tell :content result)))))


(defun process-evaluate (msg args)
  (let* ((reply-with (find-arg-in-act msg :reply-with))
	 (sender (find-arg-in-act msg :sender))
	 (content (find-arg args :content))
	 (context (find-arg args :context))
	 (result 
	  (case (car content)
	    ((adopt assertion)
	     (let* ((head (find-lf-in-context context (find-arg-in-act content :what)))
		    (headtype (find-arg head :instance-of)))
	       (format t "~% Evalutaing ~S with headtype ~S" content headtype)
	       (case headtype
		 (ont::cause-effect
		  (let* ((prop (find-lf-in-context context (find-arg head :formal)))
			 (color (find-arg prop :instance-of)))
		    (if (eq color 'ONT::GREEN)
			(list 'report :content (list 'failure :type 'CANNOT-PERFORM :what (second head)
						     :reason '(not enough green blocks SIFT to provide the details) :context context))
			(list 'report :content (list 'acceptable :what content :context context)))))
		 ;;(list 'report :content (list 'failure :type 'FAILED-TO-INTERPRET :what content :reason '(SOME-REASON) :context context)
		 (ont::create   ;; this is "let's make a N block tower"
		  (let* ((obj (find-lf-in-context context (find-arg head :affected-result)))
			 (mod (find-lf-in-context context (find-arg obj :mod)))
			 (ground (find-lf-in-context context (find-arg mod :ground)))
			 (size (find-arg ground :amount)))
		    (format t "~% size= ~S ground=~S mod = ~S" size ground mod)
		    (if (and (numberp size) (> size 4))
;		    (if (numberp size)
;			(if (> size 4)
			    (list 'report :content (list 'failure :type 'CANNOT-PERFORM :what (second head)
						     :reason '(not enough blocks SIFT to provide the details) :context context))
;			  (list 'report :content (list 'accept-with-clarify :what content :reason 'R1
;						     :context (append '((ONT::RELN R1 :INSTANCE-OF ONT::IDENTIFY :WHAT XX)
;									(ONT::THE XX :instance-of ONT::PERSON :suchthat XXX)
;								   (ONT::RELN XXX :instance-of ONT::IS-PERFORMER :figure XX))
;								      context))))
		      (list 'report :content (list 'acceptable :what content :context context))
		  
		      )))
		
		  (otherwise
		   
		   (list 'report :content (list 'acceptable :what content :context context))))
	       )))))
		  
    ;;(format t "~%========================================~% DUMMY: Received: ~S ~% Sending ~S~%==========================~%" msg result)
    (if reply-with
	(send-msg (append (list 'tell :receiver sender :content result) (list :in-reply-to reply-with)))
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
	 (act (find-lf-in-context context target))
	 (condition-id (find-arg act :content))
	 (condition (find-lf-in-context context condition-id))
	 (action (find-arg condition :action))
	 (result (find-arg condition :result))
	 )
      (format t "~%condition is ~S" condition)
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
  
	  
	))

       
      
(defun find-lf-in-context (context id)
  (find id context :key #'cadr))

