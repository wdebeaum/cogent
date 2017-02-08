;;;;
;;;; messages.lisp for PlowAgent
;;;;
;;;;
;;;; Time-stamp: <Tue Jan 17 09:23:51 EST 2017 jallen>
;;;;

(in-package :dagent)

(in-component :dagent)

(defcomponent-handler
  '(tell &key :content (cps-act-hyps . *))
  #'(lambda (msg args)
        (declare (ignore msg))
      (apply #'new-cps-act args))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (alarm . *))
  #'(lambda (msg args)
        (declare (ignore msg))
      (apply #'alarm-handler args))
  :subscribe t)


(defcomponent-handler
  '(tell &key :content (interpretation-failed . *))
  #'(lambda (msg args)
        (declare (ignore msg))
;      (apply #'utt-failure args))
	 (apply #'end-of-turn args)
	 (send-on-end-of-turn args))
  :subscribe t)

 (defcomponent-handler
  '(request &key :content (resume . *))
     #'(lambda (msg args)
           (declare (ignore msg))
	 (apply #'resume-dialogue args))
   :subscribe t)

 (defcomponent-handler
  '(tell &key :content (wizard-took-over . *))
     #'(lambda (msg args)
           (declare (ignore msg))
	 (apply #'wizard-takeover args))
   :subscribe t)

(defcomponent-handler
  '(tell &key :content (end-of-turn . *))
     #'(lambda (msg args)
           (declare (ignore msg))
	 (apply #'end-of-turn args)
	 (send-on-end-of-turn args))
   :subscribe t)

(defun send-on-end-of-turn (args)
  (send-msg `(tell :content ,(cons 'turn-finished args))))

;; ==  messages added during CWC

(defcomponent-handler
  '(tell &key :content (set-system-goal . *))
     #'(lambda (msg args)
           (declare (ignore msg))
	 (apply #'set-system-goal args))
   :subscribe t)

(defcomponent-handler
  '(tell &key :content (disable-alarms . *))
     #'(lambda (msg args)
           (declare (ignore msg))
	 (setq dagent::*using-alarms* nil))
   :subscribe t)

(defcomponent-handler
  '(tell &key :content (enable-alarms . *))
     #'(lambda (msg args)
           (declare (ignore msg))
	 (setq dagent::*using-alarms* t))
   :subscribe t)

(defcomponent-handler
  '(tell &key :content (report . *))

     #'(lambda (msg args)
	 (declare (ignore msg))
	 (handle-input-message  msg))
      
    :subscribe t)

(defcomponent-handler
    '(tell &key :content (display-available . *))
    #'(lambda (msg args)
	(send-msg (list* 'REQUEST :content (cons 'START-DISPLAY args))))
  :subscribe t)
      


;; START-CONVERSATION

(defcomponent-cancellation-pattern
    '(tell &key :content (start-conversation . *)))

(defcomponent-handler
  '(tell &key :content (start-conversation . *))
     #'(lambda (msg args)
	 (declare (ignore msg))
	 (restart-dagent)
      )
    :subscribe t)

;;; see user-db.lisp

(defun reply-to-msg-and-broadcast (msg &rest reply)
  "Send two versions of the same reply: one directly to the requester, and
   another broadcast to anyone who subscribed."
  (apply #'reply-to-msg msg reply)
  (send-msg reply))

(defcomponent-handler
  '(request &key :content (describe-user . *))
  (lambda (msg args)
    (reply-to-msg msg 'tell :content (apply #'describe-user args)))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (create-user . *))
  (lambda (msg args)
    (reply-to-msg-and-broadcast msg 'tell :content (apply #'create-user args)))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (read-users))
  (lambda (msg)
    (reply-to-msg-and-broadcast msg 'tell :content (read-users))
    )
  :subscribe t)

(defcomponent-handler
  '(request &key :content (update-user . *))
  (lambda (msg args)
    (reply-to-msg-and-broadcast msg 'tell :content (apply #'update-user args)))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (delete-user . *))
  (lambda (msg args)
    (reply-to-msg-and-broadcast msg 'tell :content (apply #'delete-user args))
    )
  :subscribe t)

(defcomponent-handler
  '(request &key :content (get-user-names-and-channel-ids))
  (lambda (msg)
    (reply-to-msg msg 'tell :content (get-user-names-and-channel-ids)))
  :subscribe t)

(defcomponent-handler
  '(request &key :content (get-wizard-names))
  (lambda (msg)
    (reply-to-msg msg 'tell :content (get-wizard-names)))
  :subscribe t)
