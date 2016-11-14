;; Add in the :trips package if it isn't there.
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
                       :name "trips")))

;;; Also make sure that the defcomponent and util libraries are loaded.
;;; Note: #!TRIPS"..." is shorthand for pathnames relative to $TRIPS_BASE.

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

;; This sets up this component using the defcomponent library, which does
;; several things for you:
;;  - It makes a Lisp package with the same name as the component (the better
;;    to intern your symbols in).
;;  - It makes a mk:defsystem with the same name (the better to load/compile
;;    your code with).
;;  - It makes an instance of a subclass of the trips-component class (by
;;    default trips-agent), with the same name, and with references to the
;;    package and the system. The trips-agent code has a message handling loop,
;;    which is why the messages.lisp file is so simple.
;;  - It defines several functions in the package to make it easier to use the
;;    trips-component instance without explicitly passing it around.
;;  - It calls in-package and in-component to put any following code in the
;;    context of the package/component.
;; See ../defcomponent/ and ../config/lisp/defsystem/ for more details.
(dfc:defcomponent :hello ; the name of the package/system/component
		  :nicknames (:hi) ; nicknames for the package
		  :use (:util :common-lisp) ; other packages used
		  :system ( ; system definition
		    :depends-on (:util) ; other systems used
		    :components ( ; files to load (without the .lisp suffix)
		      "messages"
		      )))

;; If you want to run some code before the message handling loop starts, put it
;; in a method like this:
; (dfc:defcomponent-method dfc:init-component :after ()
;   ; your code here
;   )

