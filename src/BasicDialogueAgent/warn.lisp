;;;;
;;;; warn.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 24 Sep 2003
;;;; Time-stamp: <Fri Mar 14 14:56:41 EDT 2008 ferguson>
;;;;

(in-package :dagent)

(defun da-warn (&rest args)
  (let ((msg (apply #'format nil args)))
    (logging:log-message :warn msg)
    (format *trace-output* "~&dagent: warning: ~A~%" msg)))

(defun da-debug (&rest args)
  (let ((msg (apply #'format nil args)))
    (logging:log-message :debug msg)
    (format *trace-output* "~&dagent: ~A~%" msg)))

(defvar *step* nil)
(defvar *trace-level* nil)

(defun set-debug-level (&key level)
  (case level
    (off (trace-off))
    (debug (trace-on 1))
    (otherwise (trace-on 2))))

(defun trace-on (n)
  (if (numberp n)
      (setq *trace-level* n)
    (da-warn "Step level must be a number")))

(defun trace-off nil
  (setq *trace-level* nil)
  (setq *step* nil))

(defun trace-msg (n &rest args)
  (when *trace-level*
    (when (>= *trace-level* n)
      (let ((msg (apply #'format nil args)))
	(format *trace-output* "~%DAGENT: ~A" (concatenate 'string (case n (1 "") (2 "   ") (3 "     ") (4 "       "))
			       msg))
	
	(when *step* 
	  (format *trace-output* "~%DAGENT:     at level ~S, change?:" *trace-level*)
	  (let ((x (read-line)))
	    (if (not (string= x ""))
		(let ((new (read-from-string x)))
		  (if (numberp new) (setq *trace-level* new)
		    (eval x))))))))
    (values)))

(defun trace-pprint (n msg x)
  (when (and *trace-level* (>= *trace-level* n))
    (format *trace-output* msg)
    (pprint x *trace-output*))
  (values))
