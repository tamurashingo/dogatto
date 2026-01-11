; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/utils/ulid
  (:use #:cl)
  (:export #:generate-ulid
           #:ulid-p
           #:ulid-timestamp))
(in-package #:dogatto/utils/ulid)

(defparameter *encoding* "0123456789ABCDEFGHJKMNPQRSTVWXYZ")
(defparameter *encoding-length* 32)

(defun timestamp-ms ()
  "Get current timestamp in milliseconds"
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* sec 1000) (floor usec 1000))))

(defun random-bytes (n)
  "Generate N random bytes"
  (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n)
      (setf (aref bytes i) (random 256)))
    bytes))

(defun encode-timestamp (timestamp)
  "Encode timestamp (48 bits) to 10 characters"
  (let ((result (make-string 10 :initial-element #\0)))
    (loop for i from 9 downto 0
          do (setf (char result i)
                   (char *encoding* (mod timestamp *encoding-length*)))
             (setf timestamp (floor timestamp *encoding-length*)))
    result))

(defun encode-random (random-bytes)
  "Encode random bytes (80 bits) to 16 characters"
  (let ((result (make-string 16 :initial-element #\0))
        (value 0))
    ;; Convert bytes to integer
    (loop for byte across random-bytes
          do (setf value (+ (* value 256) byte)))
    ;; Encode to base32
    (loop for i from 15 downto 0
          do (setf (char result i)
                   (char *encoding* (mod value *encoding-length*)))
             (setf value (floor value *encoding-length*)))
    result))

(defun generate-ulid ()
  "Generate a ULID string"
  (let ((timestamp (timestamp-ms))
        (random (random-bytes 10)))
    (concatenate 'string
                 (encode-timestamp timestamp)
                 (encode-random random))))

(defun ulid-p (string)
  "Check if string is a valid ULID"
  (and (stringp string)
       (= (length string) 26)
       (every (lambda (c) (find c *encoding*)) string)))

(defun ulid-timestamp (ulid)
  "Extract timestamp from ULID"
  (when (ulid-p ulid)
    (let ((timestamp-part (subseq ulid 0 10))
          (value 0))
      (loop for char across timestamp-part
            do (setf value (+ (* value *encoding-length*)
                             (position char *encoding*))))
      value)))
