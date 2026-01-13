; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/models/user
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:query
                #:execute-query
                #:make-record
                #:save
                #:ref
                #:validate
                #:setf-error)
  (:export #:<user>
           #:find-user-by-email
           #:find-user-by-id
           #:create-user
           #:user-exists-p))

(in-package #:dogatto/models/user)

(defmodel <user> (<base-model>)
  (:table "users"))

(defun find-user-by-email (email)
  "Find a user by email address.

   @param email [string] The email address to search for
   @return [<user>] The user instance if found
   @return [nil] If no user found with the given email
   "
  (first (execute-query
          (query <user>
                 :as :user
                 :where (:= (:user :email) :email))
          (list :email email))))

(defun find-user-by-id (id)
  "Find a user by ID.

   @param id [integer] The user ID to search for
   @return [<user>] The user instance if found
   @return [nil] If no user found with the given ID
   "
  (first (execute-query
          (query <user>
                 :as :user
                 :where (:= (:user :id) :id))
          (list :id id))))

(defun create-user (&key username email password-hash ulid
                         (registration-status "provisional")
                         github-id registration-token
                         registration-token-expires-at)
  "Create a new user and save it to the database.

   @param username [string] The username
   @param email [string] The email address
   @param password-hash [string] The hashed password
   @param ulid [string] The ULID for the user
   @param registration-status [string] The registration status (default: \"provisional\")
   @param github-id [string] The GitHub ID (optional)
   @param registration-token [string] The registration token (optional)
   @param registration-token-expires-at [integer] The token expiration time (optional)
   @return [<user>] The created user instance if successful
   @return [nil] If validation fails or save fails
   "
  (let ((user (make-record '<user>
                           :username username
                           :email email
                           :password-hash password-hash
                           :ulid ulid
                           :registration-status registration-status
                           :github-id github-id
                           :registration-token registration-token
                           :registration-token-expires-at registration-token-expires-at)))
    (when (save user)
      user)))

(defun user-exists-p (email)
  "Check if a user with the given email already exists.

   @param email [string] The email address to check
   @return [boolean] T if user exists, NIL otherwise
   "
  (not (null (find-user-by-email email))))

(defmethod validate ((user <user>))
  "Validate user data before saving.

   Checks:
   - username is required and not empty
   - email is required and not empty
   - ulid is required and not empty
   "
  (when (or (null (ref user :username))
            (string= (ref user :username) ""))
    (setf-error user :username "Username is required"))

  (when (or (null (ref user :email))
            (string= (ref user :email) ""))
    (setf-error user :email "Email is required"))

  (when (or (null (ref user :ulid))
            (string= (ref user :ulid) ""))
    (setf-error user :ulid "ULID is required")))
