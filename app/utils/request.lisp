; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/utils/request
  (:use #:cl)
  (:import-from #:babel
                #:octets-to-string)
  (:export #:read-body-as-string))

(in-package #:dogatto/utils/request)

(defun read-body-as-string (body-stream)
  "Read body stream as string.

   @param body-stream [stream] Request body stream
   @return [string] Body content as string
   "
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-output-to-string (out)
      (loop for bytes-read = (read-sequence buffer body-stream)
            do (write-string (babel:octets-to-string buffer :end bytes-read :encoding :utf-8) out)
            while (= bytes-read 4096)))))
