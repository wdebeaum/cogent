;;;;
;;;; File: system.lisp
;;;;
;;;; Defines and loads an instance of the TRIPS system for CABOT 
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;Systems;core;system")

(trips:def-trips-system :cogent
  (:old-trips-component :lxm               #!TRIPS"src;LexiconManager;")
  (:dfc-component	:parser            #!TRIPS"src;Parser;")
  (:dfc-component       :im                #!TRIPS"src;NewIM;")
  (:dfc-component       :dagent            #!TRIPS"src;BasicDialogueAgent;")
  ;; the :dummy component is used to fake certain message interactions during
  ;; system development. comment out the load files at the end of this file if
  ;;   you want to disable it
  (:dfc-component       :dummy		   #!TRIPS"src;Dummy;")
  )

;; add WebParser to the system when we have its source directory
(when (probe-file #!TRIPS"src;WebParser")
  (nconc (assoc :cogent trips::*trips-systems*)
	 (list '(:dfc-component :webparser #!TRIPS"src;WebParser;"))))

;; Now load the system
(trips:load-trips-system)

;; domain preferences
(load "domain-sense-preferences")
(load "domain-words.lisp")

;; and here's the state definitions for the dialogue manager

;;(load "Sallerules.lisp")

;(load "cps-states.lisp")

;;;; extractor rules
(load "cogentRules.lisp")
;(load "symbolmapping.lisp")

;;  loading the dummy message handling

;; the :dummy component is used to fake certain message interactions during
;; system development.
;; if you need to use either of the following Dummy features, uncomment them
;; LOCALLY, but please do not commit without comments!


;(load #!TRIPS"src;Systems;cogent;dummymessages.lisp")

(defun parse-eval (x)
  (im::send-msg `(request :receiver parser :content (eval ,x))))
