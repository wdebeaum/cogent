;;;;
;;;; defsys.lisp for Dagent- the generic dialogue agent 
;;;;
;;;;;;;; Time-stamp: <Tue Aug  9 23:25:14 EDT 2016 jallen>
;;;;


;; Note: this dialogue manager is driven by files that are
;;  loaded in the system folder that define valid user channels and 
;;  the dialogue manager actions. 

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :parser)
  (load #!TRIPS"src;Parser;defsys"))

(unless (find-package :IM)
  (load #!TRIPS"src;NewIM;defsys"))

(mk:defsystem :dagent-code
    :source-pathname #!TRIPS"src;BasicDialogueAgent;"
    :components ( "messages"
		  "warn"
		  "StateManager"
		  "CPSstate"
		  "user-db"
		  "cps-agent-states"
		  "basic-conversation-states"
		  ))

;;(mk:defsystem :dagent-data
   ;; :source-pathname #!TRIPS"src;BasicDialogueAgent;"
;;   :components ("states"))

(dfc:defcomponent :dagent
  :use (:common-lisp :util)
  :system (:depends-on (:util :im :parser :dagent-code)))


(dfc:defcomponent-method dfc:init-component :after ()
  (initialize)
 ;; (start-dagent)
  )


;; Dummy run -- the real component is the domain specific D-agent
(defun run ()
  (dfc:run-component :dagent)
  )

(defvar *dagent-package* (find-package :dagent))

(defvar *me*)

(defvar *user*)
;;
