;;
;; defsys.lisp for AlarmClock
;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
                       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(dfc:defcomponent :alarmclock
		  :nicknames (:alarm :ac)
                  :use (:util :common-lisp)
		  :system (
		    :depends-on (:util :comm)
		    :components (
		      "messages"
		    )))

(defconstant *seconds-per-hour* (* 60 60))
(defconstant *seconds-per-day* (* *seconds-per-hour* 24))

(defvar *alarms* nil "List of alarms (of the form (universal-time . msg)) in the order they will occur.")
(defvar *alarm-process* nil "The process that waits for alarm times and sends alarm messages (as opposed to the main process, which listens for set-alarm messages).")
(defvar *alarm-process-up-to-date* t)

(defun run ()
  (dfc:run-component :alarmclock))

