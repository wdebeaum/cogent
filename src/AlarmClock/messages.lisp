(in-package :alarmclock)
(in-component :alarmclock)

(defun send-alarms (alarms)
  "Send the alarm messages, sleeping the appropriate amount of time before each
   one."
  (loop with *package* = (find-package :alarmclock)
  	with *component* = (find-component :alarmclock)
        with last-time = (get-universal-time)
        for (alarm-time . msg) in alarms
	for sleep-time = (- alarm-time last-time)
	do
	  (when (< 0 sleep-time)
	    (format *error-output* "~&alarm process sleeping for ~s seconds in order to wake up at ~s~%" sleep-time alarm-time)
	    (sleep sleep-time))
	  (send-msg `(tell :content (alarm :msg ,msg)))
	  (setf last-time alarm-time)
	)
  ;; make sure we're alive for at least 2 seconds so update-alarm-process
  ;; doesn't hang
  (sleep 2))

(defun update-alarm-process (&optional msg)
  "Alert the alarm process to changes in *alarms*. If msg isn't given, send a
   message reminding ourselves to do this in the future (i.e. after other
   pending set-alarm messages are processed)."
  (cond
   ((null msg) ; called as a function, so maybe send ourselves a message
    (when *alarm-process-up-to-date* ; or we thought so, at least
      (sleep 1) ; let other messages pile up
      (setf *alarm-process-up-to-date* nil)
      (send-msg '(request :receiver alarmclock :content (update-alarm-process)))
      ))
   (*alarm-process-up-to-date* ; we're up to date already, do nothing
    nil)
   (t ; called as a message handler, actually update the alarm process
    (format *error-output* "~&updating alarm process~%")
    (when (and (not (null *alarm-process*))
	       (trips:process-alive-p *alarm-process*))
      (trips:process-kill *alarm-process*)
      ;; make sure it's really dead
      (loop until (not (trips:process-alive-p *alarm-process*))
	    do
	      (format *error-output* "~&waiting for alarm process to die~%") 
	      (sleep 10)
	    )
      )
    (setf *alarm-process*
	  (when *alarms*
	    ;; make sure the process is alive before we return
	    (loop with p = (trips:process-run-function
			     :alarm-sender #'send-alarms *alarms*)
		  until (trips:process-alive-p p)
		  do
		    (format *error-output* "~&waiting for alarm process to live~%")
		    (sleep 1)
		  finally (return p)
		  )))
    (setf *alarm-process-up-to-date* t)
    )
   ))

(defun same-time-tomorrow (universal-time target-hours target-minutes)
  "Add a day to the given universal-time, making sure the hour and minute is
   the same (even with a daylight savings time change)."
  (let ((tomorrow (+ universal-time *seconds-per-day*)))
    (multiple-value-bind (seconds minutes hours date month year day daylight-p zone)
        (decode-universal-time tomorrow)
	(declare (ignore minutes hours day daylight-p zone))
      (encode-universal-time seconds target-minutes target-hours date month year))))

(defun encode-hours-minutes (target-hours target-minutes)
  "Return the soonest universal time with the given values for hours and
   minutes."
  (let ((now (get-universal-time)))
    (multiple-value-bind (seconds minutes hours date month year day daylight-p zone)
	(decode-universal-time now)
	(declare (ignore seconds minutes hours day daylight-p zone))
      (loop for target = (encode-universal-time
                           0 target-minutes target-hours date month year)
		then (same-time-tomorrow target target-hours target-minutes)
	    until (> target now)
	    finally (return target)
	    ))))

(defun set-absolute-alarm (&key msg hour minute universal-time)
  "Set an alarm to happen at the either the given universal-time or the next
   occurrence of the given hour and minute."
  (unless universal-time
    (setf universal-time (encode-hours-minutes hour minute)))
  (let ((alarm (cons universal-time msg)))
    (format *error-output* "~&adding alarm ~s~%" alarm)
    (setf *alarms* (sort (cons alarm *alarms*) #'< :key #'car))
    )
  (update-alarm-process)
  )

(defun set-relative-alarm (msg delay)
  "Set an alarm to happen after the given number of hours."
  (set-absolute-alarm
    :msg msg
    :universal-time (+ (get-universal-time) (round (* delay *seconds-per-hour*)))
    ))

(defun set-alarm (message args)
    (declare (ignore message))
  (let ((delay (get-keyword-arg args :delay)))
    (if delay
      (set-relative-alarm (get-keyword-arg args :msg) delay)
      (apply #'set-absolute-alarm args)
      )))
(defcomponent-handler
  '(request &key :content (set-alarm . *))
  #'set-alarm
  :subscribe t)

(defun remove-alarm (message args)
  "Remove the alarm we just sent a message about from *alarms* so we don't send
   it again."
    (declare (ignore message))
  (setf *alarms*
        (remove (get-keyword-arg args :msg) *alarms*
	        :key #'cdr
		:test #'equalp
		:count 1
		)))
(defcomponent-handler
  '(tell &key :content (alarm . *))
  #'remove-alarm
  :subscribe t)

(defun clear-alarms (msg)
  "Cancel all alarms, or if a pattern is given, those whose messages match the
   pattern."
  (let ((pattern (find-arg-in-act msg :pattern)))
    (setf *alarms*
          (if pattern
	    nil
	    (delete pattern *alarms* :test #'dfc::match-msg-pattern))))
  (update-alarm-process)
  )
(defcomponent-handler
  '(tell &key :content (clear-alarms))
  #'clear-alarms
  :subscribe t)

(defcomponent-handler
  '(request &key :content (update-alarm-process))
  #'update-alarm-process
  :subscribe t)

