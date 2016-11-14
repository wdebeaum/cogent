;; Change this to the appropriate package.
(in-package :hello)

;; This handles all incoming hello messages. The first parameter, msg, is the
;; whole message. Later parameters are the parts of the message matched by
;; wildcards in the pattern (see below).
(defun handle-hello (msg args)
    (declare (ignore args))
  (let ((sender (find-arg-in-act msg :sender)))
    (reply-to-msg msg 'request :content
        `(hello ,@(when sender (list sender))))))

;; This sets up the handle-hello function as the handler for request messages
;; whose :content field is a list starting with hello.
(defcomponent-handler
  ;; The pattern to match messages against. "&key" means the rest of the list
  ;; is a :keyword argument list. The "*" is a wildcard, and ". *" means the
  ;; rest of the list can be anything. The parts of the message that match
  ;; wildcards are added as extra arguments to the handler function.
  '(request &key :content (hello . *))
  ;; The function to call when a matching message is received.
  #'handle-hello
  ;; This controls whether we receive messages of this type only when they're
  ;; explicitly sent to us using the :sender argument (:subscribe nil, the
  ;; default), or whenever a message of this type is sent (:subscribe t). In
  ;; the latter case, we send a subscribe message to the Facilitator with the
  ;; pattern, in order to let it know we're interested in messages of this
  ;; type.
  :subscribe t)

