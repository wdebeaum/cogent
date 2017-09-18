;;;;
;;;; defsys.lisp for DUMMY - the generic debugging agent
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :ont)
  (load #!TRIPS"src;OntologyManager;ont-pkg"))

(mk:defsystem :dummy-code
    :source-pathname #!TRIPS"src;Dummy;"
    :components ( ;;"messages"    ;; messages now in Systems
		  "code"
		  ))

(dfc:defcomponent :dummy
  :use (:common-lisp :util)
  :system (:depends-on (:util :dummy-code)))

(dfc:defcomponent-method dfc:init-component :after ()
  (initialize)
  )

;; Dummy run -- the real component is the domain specific agent
(defun run ()
  (dfc:run-component :dummy)
  )

(defun initialize ()
  (send-initial-messages))

(defvar *replyCounter* 0)
