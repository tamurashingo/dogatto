; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/middleware/session
  (:use #:cl)
  (:export #:session-middleware
           #:*session-timeout*))
(in-package #:dogatto/middleware/session)

(defparameter *session-timeout* 
  (parse-integer (clails/util:env-or-default "SESSION_TIMEOUT" "3600")))

(defun session-expired-p (last-access)
  "Check if session has expired"
  (when last-access
    (> (- (get-universal-time) last-access) *session-timeout*)))

(defun session-middleware (app)
  "Session management middleware with timeout handling"
  (lambda (env)
    (let* ((session (getf env :clack.session))
           (last-access (when session (gethash :last-access session)))
           (current-time (get-universal-time)))
      
      ;; Check session timeout
      (when (and session (session-expired-p last-access))
        ;; Clear expired session
        (clrhash session)
        (setf (gethash :expired session) t))
      
      ;; Update last access time
      (when session
        (setf (gethash :last-access session) current-time))
      
      (funcall app env))))
