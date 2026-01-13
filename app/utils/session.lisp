; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/utils/session
  (:use #:cl)
  (:import-from #:redis
                #:connect
                #:disconnect
                #:with-connection
                #:red-hset
                #:red-hgetall
                #:red-del
                #:red-expire)
  (:import-from #:uuid
                #:make-v4-uuid
                #:print-bytes)
  (:import-from #:local-time
                #:now
                #:timestamp-to-unix
                #:unix-to-timestamp
                #:timestamp<)
  (:export #:generate-session-id
           #:create-session
           #:get-session
           #:delete-session
           #:session-valid-p))
(in-package #:dogatto/utils/session)

(defparameter *default-session-ttl* (* 7 24 60 60)
  "Default session TTL in seconds (7 days)")

(defun get-redis-host ()
  "Get Redis host from environment variables.
   
   @return [string] Redis host
   "
  (or (uiop:getenv "REDIS_HOST") "localhost"))

(defun get-redis-port ()
  "Get Redis port from environment variables.
   
   @return [integer] Redis port
   "
  (parse-integer (or (uiop:getenv "REDIS_PORT") "6379")))

(defun generate-session-id ()
  "Generate a unique session ID using UUID v4.
   
   @return [string] UUID string (e.g., \"550e8400-e29b-41d4-a716-446655440000\")
   "
  (format nil "~(~a~)" (make-v4-uuid)))

(defun session-key (session-id)
  "Generate Redis key for session data.
   
   @param session-id [string] Session ID
   @return [string] Redis key (e.g., \"session:550e8400-...\")
   "
  (format nil "session:~a" session-id))

(defun create-session (user-id &key (ttl *default-session-ttl*))
  "Create a new session and store it in Redis.
   
   Creates a session with the given user ID and stores it in Redis.
   The session includes user-id, created-at, and expires-at timestamps.
   
   @param user-id [integer] User ID to associate with session
   @param ttl [integer] Time-to-live in seconds (default: 7 days)
   @return [string] Session ID
   @condition redis-error Redis connection or operation failed
   "
  (let* ((session-id (generate-session-id))
         (key (session-key session-id))
         (now (now))
         (created-at (timestamp-to-unix now))
         (expires-at (+ created-at ttl)))
    (with-connection (:host (get-redis-host) :port (get-redis-port))
      ;; Store session data as hash
      (red-hset key "user-id" (write-to-string user-id))
      (red-hset key "created-at" (write-to-string created-at))
      (red-hset key "expires-at" (write-to-string expires-at))
      ;; Set expiration
      (red-expire key ttl))
    session-id))

(defun get-session (session-id)
  "Retrieve session data from Redis.
   
   @param session-id [string] Session ID to retrieve
   @return [plist] Session data as plist (:user-id :created-at :expires-at)
   @return [nil] If session does not exist
   @condition redis-error Redis connection or operation failed
   "
  (let ((key (session-key session-id)))
    (with-connection (:host (get-redis-host) :port (get-redis-port))
      (let ((data (red-hgetall key)))
        (when data
          ;; Convert Redis hash to plist
          ;; red-hgetall returns a list like ("user-id" "123" "created-at" "12345" ...)
          (let ((user-id (cadr (member "user-id" data :test #'string=)))
                (created-at (cadr (member "created-at" data :test #'string=)))
                (expires-at (cadr (member "expires-at" data :test #'string=))))
            (when (and user-id created-at expires-at)
              (list :user-id (parse-integer user-id)
                    :created-at (parse-integer created-at)
                    :expires-at (parse-integer expires-at)))))))))
(defun delete-session (session-id)
  "Delete a session from Redis.
   
   @param session-id [string] Session ID to delete
   @return [boolean] T if session was deleted, NIL if it didn't exist
   @condition redis-error Redis connection or operation failed
   "
  (let ((key (session-key session-id)))
    (with-connection (:host (get-redis-host) :port (get-redis-port))
      (> (red-del key) 0))))

(defun session-valid-p (session-id)
  "Check if a session is valid (exists and not expired).
   
   @param session-id [string] Session ID to check
   @return [boolean] T if session is valid, NIL otherwise
   @condition redis-error Redis connection or operation failed
   "
  (let ((session-data (get-session session-id)))
    (when session-data
      (let* ((expires-at (getf session-data :expires-at))
             (now (timestamp-to-unix (now))))
        (< now expires-at)))))
