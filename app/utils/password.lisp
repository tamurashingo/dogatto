; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/utils/password
  (:use #:cl)
  (:import-from #:ironclad
                #:pbkdf2-hash-password
                #:pbkdf2-check-password
                #:make-random-salt)
  (:import-from #:babel
                #:string-to-octets
                #:octets-to-string)
  (:export #:hash-password
           #:verify-password
           #:validate-password))
(in-package #:dogatto/utils/password)

(defun hash-password (password)
  "Hashes the given password using PBKDF2 with SHA256.

   Takes a plain text password and returns a secure hash that can be
   safely stored in the database. Uses PBKDF2 with 10000 iterations
   and a random salt for each password.

   @param password [string] The plain text password to hash
   @return [string] Base64-encoded hash with embedded salt
   "
  (let* ((password-octets (string-to-octets password :encoding :utf-8))
         (salt (make-random-salt))
         (hash (pbkdf2-hash-password password-octets
                                     :salt salt
                                     :digest :sha256
                                     :iterations 10000)))
    (ironclad:byte-array-to-hex-string hash)))

(defun verify-password (password hash)
  "Verifies a password against a stored hash.

   Compares the given plain text password with the stored hash.
   Returns T if the password matches, NIL otherwise.

   @param password [string] The plain text password to verify
   @param hash [string] The stored password hash
   @return [boolean] T if password matches, NIL otherwise
   "
  (handler-case
      (let* ((password-octets (string-to-octets password :encoding :utf-8))
             (stored-hash (ironclad:hex-string-to-byte-array hash)))
        (pbkdf2-check-password password-octets stored-hash))
    (error () nil)))

(defun validate-password (password)
  "Validates password strength.

   Checks if the password meets minimum security requirements:
   - At least 8 characters long
   - Contains at least one letter
   - Contains at least one number

   @param password [string] The password to validate
   @return [boolean] T if password is valid, NIL otherwise
   @return [list] List of error messages if validation fails
   "
  (let ((errors '()))
    ;; Check minimum length
    (when (< (length password) 8)
      (push "Password must be at least 8 characters long" errors))
    
    ;; Check for at least one letter
    (unless (find-if #'alpha-char-p password)
      (push "Password must contain at least one letter" errors))
    
    ;; Check for at least one number
    (unless (find-if #'digit-char-p password)
      (push "Password must contain at least one number" errors))
    
    (if (null errors)
        (values t nil)
        (values nil (nreverse errors)))))
