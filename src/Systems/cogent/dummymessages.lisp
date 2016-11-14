;;;;
;;;; messages.lisp for DUMMY
;;;;
;;;;

(in-package :dummy)

(in-component :dummy)

;;  Here we pick up messages for all modules that don't exist yet, to
;;  allow developers to run the system as they add functionality
;;  As functionality is added, messages should be commented out


;;=================
;;  BA interactions
;;=================

(defcomponent-handler
    '(request &key :content (what-next . *))
    #'(lambda (msg args)
	(let ((active-goal (find-arg args :active-goal))
	      (context (remove-minus (find-arg args :context))))
	  (format t "~%~%args = ~S; active-goal = ~S" args active-goal)
	  (process-reply msg args 
			 (let ((action (find-lf-in-context context active-goal)))
			   (case (find-arg action :instance-of)
			     (ont::startoff-begin-commence-start
			      '(REPLY :content (PERFORM :agent *USER* :action A2)
				:context ((ONT::RELN A2 :instance-of ONT::START)
					  )
				))

			     (ont::execute
			      `(REPORT
				:content (FAILURE
					  :type CANNOT-PERFORM
					  :what ,active-goal
					  :reason DONT-KNOW-ANYTHING
					  :possible-resolution ((ADOPT :what G5 :AS (GOAL))))
				:context ((ONT::RELN A5 :instance-of ONT::EXECUTE :AGENT *USER* :NEUTRAL S5)
					  (ONT::PRO S5 :instance-of ONT::REFERENTIAL-SEM)
					  )
				))
			     
			     (otherwise
			      '(REPLY :content (WAIT)))
			     )))))
  :subscribe t)

(defun remove-minus (x)
  (if (not (eq x '-))  x))

(defcomponent-handler
  '(request &key :content (accepted . *))
     #'(lambda (msg args)
	 (process-reply msg args
			'(ONT::OK)))
  :subscribe t)

#||(defcomponent-handler
  '(request &key :content (notify-completed . *))
     #'(lambda (msg args)
	 (process-reply msg args
				 '(ONT::OK)))
  :subscribe t)||#



;;=================
;;  BA interaction: eval/commit cycle -- this one rule picks up any evaluate and sends back an acceptance
;;=================

(defcomponent-handler
  '(request &key :content (evaluate  . *))
     #'(lambda (msg args)
	 (process-evaluate msg args))
  :subscribe t)


