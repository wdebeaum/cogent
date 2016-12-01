AlarmClock
William de Beaumont
$Date: 2011/11/14 16:27:09 $

This is a simple alarm clock; you can set alarms to happen at a certain hour
and minute, or after a certain number of hours. Each alarm happens only once.
Note that since it sends messages to itself, it needs to be connected to the
Facilitator in order to operate correctly.

KQML usage examples:

;; set an alarm to happen at 6pm local time:
(request :content (set-alarm :msg "It's 6pm!" :hour 18 :minute 0))
;; then at 6pm:
(tell :content (alarm :msg "It's 6pm!") :sender AlarmClock)

;; set an alarm to happen 3 hours from now:
(request :content (set-alarm :msg (arbitrary (message :here 42)) :delay 3))

;; clear all pending alarms:
(request :content (clear-alarms))


Implementation notes:

The AlarmClock module uses two threads: one for processing incoming messages,
and another for waiting and sending alarm messages. The message processing
thread synchronizes with the alarm sending thread (*alarm-process*) by killing
it and restarting it, passing it the updated list of *alarms*. The message
processing thread also listens for AlarmClock's own alarm messages, in order to
remove them from *alarms* and avoid sending them again.

Often AlarmClock will receive several set-alarm messages at once. Previously,
this meant killing and restarting the *alarm-process* several times in rapid
succession. It was seen that this often resulted in multiple messages being
sent for the same alarms, since the threads weren't dying immediately. In order
to avoid this, on the first set-alarm message in a clump of them, AlarmClock
waits 1 second and sends itself an update-alarm-process message. This gets
queued after the clump, so the *alarm-process* only gets updated once per
clump, at most once per second.
