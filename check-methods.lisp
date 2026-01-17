(ql:quickload :dogatto :silent t)
(in-package :cl-user)
(format t "~%=== Methods for do-post ===~%")
(let* ((gf (find-symbol "DO-POST" "CLAILS/CONTROLLER/BASE-CONTROLLER"))
       (methods (when gf (sb-mop:generic-function-methods (symbol-function gf)))))
  (if methods
      (dolist (m methods)
        (format t "~A~%" (mapcar (function class-name) (sb-mop:method-specializers m))))
      (format t "No methods found or function does not exist~%")))
