; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/utils/time-conversion
  (:use #:cl)
  (:export #:universal-time-to-unix-time
           #:unix-time-to-universal-time))

(in-package #:dogatto/utils/time-conversion)

(defun universal-time-to-unix-time (universal-time)
  "Convert Common Lisp Universal Time to Unix timestamp.

   Universal Time: seconds since 1900-01-01 00:00:00 UTC
   Unix Time: seconds since 1970-01-01 00:00:00 UTC
   Difference: 2208988800 seconds (70 years)

   @param universal-time [integer] Universal Time
   @return [integer] Unix timestamp
   @return [nil] If universal-time is nil
   "
  (when universal-time
    (- universal-time 2208988800)))

(defun unix-time-to-universal-time (unix-time)
  "Convert Unix timestamp to Common Lisp Universal Time.

   Unix Time: seconds since 1970-01-01 00:00:00 UTC
   Universal Time: seconds since 1900-01-01 00:00:00 UTC
   Difference: 2208988800 seconds (70 years)

   @param unix-time [integer] Unix timestamp
   @return [integer] Universal Time
   @return [nil] If unix-time is nil
   "
  (when unix-time
    (+ unix-time 2208988800)))
