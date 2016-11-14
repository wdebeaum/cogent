;;;;
;;;; messages.lisp for DUMMY
;;;;
;;;;

(in-package :dummy)

(in-component :dummy)

;;  Here we pick up messages for all modules that don't exist yet, to
;;    allow developers to run the system as they add functionality
;;   As functionality is added, messages should be commented out


;;=================
;;  BA interactions
;;=================

(defcomponent-handler
    '(request &key :content (SET-SHARED-GOAL . *))
    #'(lambda (msg args)
	(process-reply  msg args
				  '(ONT::OK)))
  :subscribe t)

(defcomponent-handler
    '(request &key :content (what-next . *))
    #'(lambda (msg args)
	(let ((active-goal (find-arg args :active-goal)))
	  (format t "~%~%args = ~S; active-goal = ~S" args active-goal)
	  (process-reply msg args 
			 (when (consp active-goal)
			   (case (fourth active-goal)
			     (ont::BUILD
			      '(ONT::PERFORM :agent *USER*
				:action (A A1 :instance-of ONT::PUT :agent *USER* :affected b1 :result on1
					 :suchthat
					 ((RELN on1 :instance-of ont::ON :of b1 :val t1)
					  (A b1 :instance-of ont::BLOCK)
					  (THE t1 :instance-of ont::TABLE)))))
			     ;;  there are now two blocks on the table, say b1 and b2
			     ;;   this is something like "move b1 until it touches b2" (a simpler version of "push them together")
			     (ont::PUT
			      '(ONT::PERFORM :agent *USER*
				:action (A A2 :instance-of ONT::PUSH :agent *USER* :affected b1 :result to1
					 :suchthat
					 ((RELN to1 :instance-of ont::TOUCH :neutral b1 :neutral1 b2)
					  ))))
			     (ont::PUSH
			      '(ONT::PERFORM :agent *USER*
				:action (A A3 :instance-of ONT::MAKE-IT-SO :agent *USER*  :result on2
					 :suchthat
					 ((RELN on2 :instance-of ont::ON :of b3 :val b2 :suchthat
						((A b3 :instance-of ONT::BLOCK)))))
					  ))
			     (ont::MAKE-IT-SO
			      '(ONT::DONE))
			     ;;  just to give some reply - we didn't anticipate the call
			     (otherwise
			      '(ONT::ACT))
			     )))))
			 
  :subscribe t)

(defcomponent-handler
  '(request &key :content (accepted . *))
     #'(lambda (msg args)
	 (process-reply msg args
				 '(ONT::OK)))

  :subscribe t)

(defcomponent-handler
  '(request &key :content (notify-completed . *))
     #'(lambda (msg args)
	 (process-reply msg args
				 '(ONT::OK)))
  :subscribe t)

(defcomponent-handler
    '(tell &key :content (update-world . *))
    #'(lambda (msg args)
	(let ((prop (find-arg (car (find-arg args :content)) :instance-of))
	      (force (find-arg (car (find-arg args :content)) :force)))
	  (process-reply msg args
			 (case prop
			   (ont::touch
			    (if (eq force 'ont::true)
				'(EXECUTION-STATUS :action A2 :status ont::DONE)
				'(EXECUTION-STATUS :action A2 :status ont::INCOMPLETE)))
			   (ont::on
			    '(EXECUTION-STATUS :action A3 :status ont::DONE))
			   )
			 )))
			   
				
  :subscribe t)
