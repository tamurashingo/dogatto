; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/utils/validation
  (:use #:cl)
  (:export #:validate-email
           #:validate-required
           #:validate-length
           #:validate-ulid
           #:collect-validation-errors))
(in-package #:dogatto/utils/validation)

(defun validate-email (email)
  "Validate email format. Returns T if valid, NIL otherwise."
  (and (stringp email)
       (> (length email) 3)
       (ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" email)))

(defun validate-required (value field-name)
  "Check if value is not nil or empty. Returns error message or NIL."
  (when (or (null value)
            (and (stringp value) (zerop (length (string-trim " " value)))))
    (format nil "~A is required" field-name)))

(defun validate-length (value field-name &key min max)
  "Validate string length. Returns error message or NIL."
  (unless (stringp value)
    (return-from validate-length (format nil "~A must be a string" field-name)))
  (let ((len (length value)))
    (cond
      ((and min (< len min))
       (format nil "~A must be at least ~A characters" field-name min))
      ((and max (> len max))
       (format nil "~A must be at most ~A characters" field-name max))
      (t nil))))

(defun validate-ulid (ulid field-name)
  "Validate ULID format. Returns error message or NIL."
  (unless (dogatto/utils/ulid:ulid-p ulid)
    (format nil "~A must be a valid ULID" field-name)))

(defun collect-validation-errors (&rest validators)
  "Collect all non-nil validation error messages.
   Usage: (collect-validation-errors 
            (validate-required email 'email')
            (validate-email email))"
  (remove nil validators))
