(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

(load #!TRIPS"src;dummy;defsys")

(mk:load-system :dummy)


