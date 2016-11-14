;;;;
;;;; plow.lisp : Load the TRIPS Behavioral Agent
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu, 20 Jun 2000
;;;; Time-stamp: <Mon Sep 12 17:33:49 EDT 2011 jallen>
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;BasicDialogAgent;defsys")

(mk:load-system :dagent)


