; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/helpers/json-converters
  (:use #:cl)
  (:import-from #:clails/model
                #:ref)
  (:import-from #:dogatto/models/todo-tag
                #:find-tags-for-todo)
  (:import-from #:dogatto/utils/time-conversion
                #:universal-time-to-unix-time)
  (:export #:todo-to-json
           #:tag-to-json
           #:label-to-json
           #:user-to-json))

(in-package #:dogatto/helpers/json-converters)

(defun tag-to-json (tag)
  "Convert tag instance to JSON-friendly alist.

   Provides a complete representation including timestamps for general use.

   @param tag [<tag>] Tag instance
   @return [list] Alist representation of tag
   "
  (list (cons "id" (ref tag :id))
        (cons "ulid" (ref tag :ulid))
        (cons "name" (ref tag :name))
        (cons "color" (ref tag :color))
        (cons "createdAt" (ref tag :created-at))
        (cons "updatedAt" (ref tag :updated-at))))

(defun tag-to-json-simple (tag)
  "Convert tag instance to simplified JSON-friendly alist.

   Provides a minimal representation without timestamps for nested use.

   @param tag [<tag>] Tag instance
   @return [list] Alist representation of tag
   "
  (list (cons "ulid" (ref tag :ulid))
        (cons "name" (ref tag :name))
        (cons "color" (ref tag :color))))

(defun todo-to-json (todo &key owner-id)
  "Convert todo model to JSON-safe alist.

   Includes associated tags if owner-id is provided.

   @param todo [<todo>] TODO model instance
   @param owner-id [integer] Owner ID for fetching tags (optional)
   @return [list] Alist representation of TODO
   "
  (let* ((content-val (ref todo :content))
         (completed-at-val (ref todo :completed-at))
         (due-date-val (ref todo :due-date))
         (tags (when owner-id
                 (find-tags-for-todo (ref todo :ulid) owner-id)))
         (tags-json (mapcar #'tag-to-json-simple tags)))
    (list (cons "id" (ref todo :id))
          (cons "ulid" (ref todo :ulid))
          (cons "ownerId" (ref todo :owner-id))
          (cons "title" (ref todo :title))
          (cons "content" content-val)
          (cons "dueDate" (if (or (null due-date-val) (zerop due-date-val))
                              :null
                              (universal-time-to-unix-time due-date-val)))
          (cons "status" (ref todo :status))
          (cons "completedAt" (if (or (null completed-at-val) (zerop completed-at-val))
                                  :null
                                  (universal-time-to-unix-time completed-at-val)))
          (cons "createdAt" (universal-time-to-unix-time (ref todo :created-at)))
          (cons "updatedAt" (universal-time-to-unix-time (ref todo :updated-at)))
          (cons "tags" tags-json))))

(defun label-to-json (label tag-count todo-count &optional tags)
  "Convert label instance to JSON-friendly alist.

   @param label [<label>] Label instance
   @param tag-count [integer] Number of tags associated with label
   @param todo-count [integer] Number of TODOs matching label
   @param tags [list] Optional list of tag instances
   @return [list] Alist representation of label
   "
  (let ((base-json (list (cons "id" (ref label :id))
                         (cons "ulid" (ref label :ulid))
                         (cons "name" (ref label :name))
                         (cons "description" (ref label :description))
                         (cons "tagCount" tag-count)
                         (cons "todoCount" todo-count)
                         (cons "createdAt" (ref label :created-at))
                         (cons "updatedAt" (ref label :updated-at)))))
    (if tags
        (append base-json (list (cons "tags" (mapcar #'tag-to-json-simple tags))))
        base-json)))

(defun user-to-json (user)
  "Convert user model to JSON-safe alist without password hash.

   @param user [<user>] User model instance
   @return [list] Alist representation of user data
   "
  (list (cons "id" (ref user :id))
        (cons "name" (ref user :username))
        (cons "email" (ref user :email))
        (cons "ulid" (ref user :ulid))
        (cons "registrationStatus" (ref user :registration-status))
        (cons "createdAt" (ref user :created-at))
        (cons "updatedAt" (ref user :updated-at))))
