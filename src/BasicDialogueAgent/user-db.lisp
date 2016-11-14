(in-package :dagent)

(defvar *users-file* #!TRIPS"etc/users.lisp" "Pathname of the file to read and write users to/from.")

(defun describe-alarm-simply (alarm)
    (declare (type alarm alarm))
  "Turn an alarm struct made by make-alarm-simply back into a list whose
   arguments can be fed back into that function to make the same struct."
  (with-slots (time args start-state) alarm
    (ecase start-state
      (start-of-day
        `(alarm :type good-morning :time ,time))
      (MS1
        `(alarm :type morning-questions :time ,time))
      (control-med1
        `(alarm :type controller-med :time ,time
	        :controller-med ,(find-arg args :name)))
      (end-of-day
        `(alarm :type evening-questions :time ,time)
      ))))

(defun describe-user (&key user name channel-id)
  "Return an S-expression completely describing a user in the database, given
   one of the following:
   :name - the name of the user
   :channel-id - the channel we communcate with the user on
   :user - one of the above two, or a user struct"
  (typecase user
    (null
      (setf user (lookup-user (or name channel-id))))
    ((or string symbol)
      (setf user (lookup-user user)))
    (user nil)
    (otherwise
      (error "expected user struct or user identifier, but got ~S" user))
    )
  (unless (typep user 'user)
    (error "Failed to look up user"))
  (with-slots (name channel-id role wizard parent-channel-id daily-report-to
               local-kb alarms)
  	      user
    (let ((triggers
            (mapcan
	      (lambda (e)
	        (when (eq :trigger (car e))
		  (list (second e))))
	      local-kb))
	  (quick-relief-meds
            (mapcan
	      (lambda (e)
	        (when (eq :quick-relief-med (car e))
		  (list (second e))))
	      local-kb)))
    `(user :name ,name :channel-id ,channel-id
      ,@(when role `(:role ,role))
      ,@(when wizard `(:wizard ,wizard))
      ,@(when parent-channel-id `(:parent-channel-id ,parent-channel-id))
      ,@(when daily-report-to `(:daily-report-to ,daily-report-to))
      ,@(when triggers `(:triggers ,triggers))
      ,@(when quick-relief-meds `(:quick-relief-meds ,quick-relief-meds))
      ,@(when alarms `(:alarms ,(mapcar #'describe-alarm-simply alarms)))
      :active ,(not (null (member name *active-users* :test #'string-equal)))
      ))))

(defun write-users (&optional (users-file *users-file*))
  "Write users to *users-file*, so that it may be read by read-users later."
  (with-open-file (s users-file :direction :output :if-exists :supersede)
    (loop with *package* = (find-package :dagent)
    	  with *print-pretty* = t
	  for name-user in (sort (copy-list *users*) #'string< :key #'car)
	  for name = (car name-user)
	  for user = (cdr name-user)
	  when (and (stringp name) (string-equal name (user-name user)))
	  do (format s "~S~%~%" (describe-user :user user)))))

(defun make-alarm-simply (&key type time controller-med nth-dose)
  (ecase type
    (good-morning
      (alarm :time time
      	     :test '(no-contact-today-yet)
	     :start-state 'start-of-day))
    (morning-questions
      (alarm :time time
             :test '(null (attr morning-woken))
	     :start-state 'MS1
	     :persistent t))
    (controller-med
      (unless controller-med
        (error "you must specify a controller-med to ask about"))
      (case nth-dose
        ((0 nil)
	  (alarm :time time
		 :test `(null (attr-contains controller-med ,controller-med))
		 :args `(:name ,controller-med)
		 :start-state 'control-med1
		 :persistent t))
	(1
	  (alarm :time time
		 :test `(only-one (attr-contains controller-med ,controller-med))
		 :args `(:name ,controller-med)
		 :start-state 'control-med1
		 :persistent t))
	(2
	  (alarm :time time
		 :test `(only-two (attr-contains controller-med ,controller-med))
		 :args `(:name ,controller-med)
		 :start-state 'control-med1
		 :persistent t))
	(t
	  (error "more than 3 doses/day unimplemented"))
	))
    (evening-questions
      (alarm :time time
	     :test '(null (attr email-sent))
	     :start-state 'end-of-day
	     :persistent t))
    ))

(defun alarm-spec-lessp (a b)
  "Is alarm spec list a at an earlier time than alarm spec list b?"
  (let* ((at (find-arg-in-act a :time))
	 (ah (first at))
	 (am (second at))
	 (bt (find-arg-in-act b :time))
	 (bh (first bt))
	 (bm (second bt)))
    (cond
      ((< ah bh) t)
      ((> ah bh) nil)
      ((< am bm) t)
      (t nil)
      )))

(defun make-alarms-simply (alarms)
  ;; look through alarms in order, setting nth-dose on controller-med alarms
  (setf alarms (sort (copy-tree alarms) #'alarm-spec-lessp))
  (loop with cm-to-nth = nil
        for alarm-spec in alarms
	when (eq 'controller-med (find-arg-in-act alarm-spec :type))
	do
	  (let* ((cm (find-arg-in-act alarm-spec :controller-med))
		 (nth (assoc cm cm-to-nth :test #'string-equal)))
	    (unless nth
	      (setf nth (cons cm 0))
	      (push nth cm-to-nth))
	    (nconc alarm-spec `(:nth-dose ,(cdr nth)))
	    (incf (cdr nth))
	    )
	)
  ;; convert alarm specs to structs
  (mapcar
    (lambda (alarm-spec)
      (apply #'make-alarm-simply (cdr alarm-spec)))
    alarms)
  )

(defun create-user (&key name channel-id role wizard parent-email parent-channel-id daily-report-to triggers quick-relief-meds alarms (active t) initial-read)
    (declare (type (or null string) name)
    	     (type (or null symbol string) channel-id)
	     (type symbol role)
	     (type (or null string) wizard parent-email parent-channel-id)
	     (type list daily-report-to triggers quick-relief-meds alarms)
	     (type boolean active)
	     (type boolean initial-read)
	     )
  "Create a new user, adding them both to the in-memory *users* list and to the
   on-disk *users-file*. Also set alarms for the new user, if they're active."
  (unless name (error "name required"))
  (unless role (error "role required"))
  (unless channel-id (error "channel-id required"))
  ; TODO make sure wizard exists
  (when (lookup-user name)
    (return-from create-user `(user-already-created :name ,name)))
  (when (lookup-user channel-id)
    (return-from create-user `(user-already-created :channel-id ,channel-id)))
  (define-user name
      (make-user
	:name name
	:channel-id channel-id
	:role role
	:wizard wizard
	:parent-channel-id (or parent-channel-id
			       ;; TODO take old parent-email out once
			       ;; users.lisp is updated
			       (when parent-email
			         (concatenate 'string "mailto:" parent-email)))
	:daily-report-to daily-report-to
	:local-kb
	  (append
	    (mapcar (lambda (m) `(:trigger ,m)) triggers)
	    (mapcar (lambda (m) `(:quick-relief-med ,m)) quick-relief-meds)
	    )
	:alarms (make-alarms-simply alarms)
	))
  (when active
    (push name *active-users*)
    (unless initial-read
      (newday name))
    )
  (unless initial-read
    (write-users))
  `(user-created :name ,name :channel-id ,channel-id)
  )

(defun read-users (&optional (users-file *users-file*))
  "Read users from *users-file*, previously written by write-users. Previous
   user information in memory will be cleared."
  (when *active-users*
    (send-msg `(request :content (clear-alarms :pattern (alarm &key :user *)))))
  (setf *active-users* nil
        *users* nil)
  (with-open-file (s users-file :direction :input)
    (loop with *package* = (find-package :dagent)
          for user-desc = (read s nil)
          while user-desc
	  do
	    (unless (and (listp user-desc) (eq 'user (car user-desc)))
	      (error "expected user description, but got ~S" user-desc))
	    (apply #'create-user `(:initial-read t ,@(cdr user-desc)))
	  ))
  `(users-read ,(get-user-names-and-channel-ids))
  )

(defun depluralize-keyword (k)
  "Assuming k is a keyword with a plural S at the end of its name, return a
   version of k with the S removed."
  (let* ((plural-name (symbol-name k))
	 (singular-name (subseq plural-name 0 (- (length plural-name) 1))))
    (intern singular-name :keyword)))

(defun update-user (&rest all-args &key old-name old-channel-id name channel-id role wizard parent-channel-id daily-report-to triggers quick-relief-meds alarms active)
    (declare (type (or null string) old-name name)
    	     (type (or null symbol string) old-channel-id channel-id)
	     (type (or null symbol) role)
	     (type (or null string) wizard parent-channel-id)
	     (type list daily-report-to triggers quick-relief-meds alarms)
	     (type boolean active)
	     ;; use all-args for these
	     (ignore name role wizard parent-channel-id daily-report-to triggers quick-relief-meds)
	     )
  "Change the slot values of a user identified by either old-name or
   old-channel-id. Apply this change both to the in-memory *users* list and to
   the on-disk *users-file*. Also let AlarmClock know about changes if
   appropriate."
  (let* ((user (lookup-user (or old-name old-channel-id)))
	 (old-name (user-name user))
	 (old-channel-id (user-channel-id user)))
    (when (and (member old-name *active-users* :test #'string-equal)
               (or alarms channel-id))
      ;; clear all this user's alarms
      (send-msg `(request :content (clear-alarms :pattern (alarm &key :user ,old-channel-id)))))
    (loop for rest-of-args = all-args
              then (cddr rest-of-args)
	  for key = (car rest-of-args)
	  for val = (cadr rest-of-args)
	  while rest-of-args
	  do
	    (ecase key
	      ((:old-name :old-channel-id) nil)
	      ((:name :channel-id)
		(when (eq key :name)
		  ;; replace old name with new name in *active-users*
		  (setf *active-users*
		        (nsubstitute val old-name *active-users*
				     :test #'string-equal))
		  (when (eq 'wizard (user-role user))
		    ;; replace name in other users' wizard slot
		    (dolist (u *users*)
		      (when (string-equal old-name (user-wizard (cdr u)))
			(setf (user-wizard (cdr u)) val))))
		  )
		      ;; replace old name/channel-id with new in *users*
	        (setf *users*
		      (cons (cons val user)
		            (delete (if (eq key :name) old-name old-channel-id)
			            *users* :key #'car :test #'string-equal))
		      ;; replace old with new in the user structure itself
	              (slot-value user (intern (symbol-name key) :dagent)) val)
		)
	      ((:role :wizard :parent-channel-id :daily-report-to)
	        (setf (slot-value user (intern (symbol-name key) :dagent)) val))

	      ((:triggers :quick-relief-meds)
	        ;; replace just the trigger/QRMs in local-kb, keep other entries
		(let ((singular-key (depluralize-keyword key)))
		  (setf (user-local-kb user)
			(append (delete singular-key (user-local-kb user)
					:key #'car)
				(mapcan
				  (lambda (m)
				    `((,singular-key ,m)))
				  val)))))
	      (:alarms
	        (setf (user-alarms user) (make-alarms-simply alarms)))
	      (:active
	        (let* ((name (user-name user))
		       (old-active (member name *active-users*
		       			   :test #'string-equal)))
	          (cond
		    ((and active (not old-active))
		      (push name *active-users*))
		    ((and (not active) old-active)
		      (setf *active-users*
		            (delete name *active-users* :test #'string-equal)))
		    )))
	      )
	  )
    (when (and (member (user-name user) *active-users* :test #'string-equal)
               (or alarms channel-id))
      ;; set new alarms, or same alarms with new channel-id
      (set-alarms user))
    (write-users)
    `(user-updated :old-name ,old-name :old-channel-id ,old-channel-id :new-name ,(user-name user) :new-channel-id ,(user-channel-id user))
    )
  )

(defun delete-user (&key name channel-id)
  "Remove a user completely from the DB. To deactivate a user but leave their
   entry in the DB, use update-user with :active nil."
  (let* ((user (lookup-user (or name channel-id)))
         (name (user-name user))
	 (channel-id (user-channel-id user)))
    (when (and (eq 'wizard (user-role user))
               (some (lambda (u) (string-equal name (user-wizard (cdr u)))) *users*))
      (error "can't delete wizard user because some other users still have them as their wizard"))
    (when (member name *active-users* :test #'string-equal)
      (send-msg `(request :content (clear-alarms :pattern (alarm &key :user ,channel-id)))))
    (setf *active-users* (delete name *active-users* :test #'string-equal))
    (setf *users*
      (delete-if (lambda (key)
                   (or (string-equal name key) (string= channel-id key)))
                 *users*
		 :key #'car))
    (write-users)
    `(user-deleted :name ,name :channel-id ,channel-id)
    )
  )

(defun get-user-names-and-channel-ids ()
  "Return a list of (name channel-id) pairs for all users."
  (loop for key-user in *users*
	for key = (car key-user)
	for user = (cdr key-user)
	when (string-equal key (user-name user))
	collect (list (user-name user) (user-channel-id user))))

(defun get-wizard-names ()
  "Return a list of names of users with the wizard role."
  (loop for key-user in *users*
        for key = (car key-user)
	for user = (cdr key-user)
	when (and (string-equal key (user-name user)) ; key is the name
	          (eq 'wizard (user-role user))) ; user is a wizard
	collect key))

