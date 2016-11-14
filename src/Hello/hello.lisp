;; Load trips if not yet loaded.
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
                       :name "trips")))

;; Load the defsys.lisp file; make sure to change this to the right path.
(load #!TRIPS"src;Hello;defsys")

;; Load the component defined in defsys.
(dfc:load-component :hello)

;; Define a more convenient way to run the component.
;; Note: by default this connects to the Lisp console; to make it connect to
;; the Facilitator via a socket, evaluate this first:
;;   (setf io::*transport* :socket)
(defun run ()
  (dfc:run-component :hello))

