
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
                       :name "trips")))

(load #!TRIPS"src;AlarmClock;defsys")

(mk:load-system :alarmclock)

