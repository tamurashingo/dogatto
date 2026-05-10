; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/todo-tags-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/services/todo-service
                #:get-todo-tags
                #:assign-todo-tags
                #:remove-todo-tag)
  (:import-from #:dogatto/helpers/auth-helper
                #:get-authenticated-user)
  (:import-from #:dogatto/helpers/json-converters
                #:tag-to-json-simple)
  (:import-from #:clails/model
                #:ref)
  (:export #:<todo-tags-controller>))

(in-package #:dogatto/controllers/todo-tags-controller)

(defclass <todo-tags-controller> (<rest-controller>)
  ()
  (:documentation "Controller for managing TODO tags (GET /todos/:ulid/tags, PUT /todos/:ulid/tags)"))

(defmethod do-get ((controller <todo-tags-controller>))
  "Get all tags for a TODO."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (param controller "ulid"))
           (result (get-todo-tags ulid (ref user :id))))
      
      (if (getf result :success)
          (let* ((tags (getf result :tags))
                 (tags-json (mapcar #'tag-to-json-simple tags)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tags" . ,tags-json))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

(defmethod do-put ((controller <todo-tags-controller>))
  "Assign tags to a TODO (replaces existing tags)."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (param controller "ulid"))
           (tag-ulids (param controller "tagUlids"))
           (result (assign-todo-tags ulid (or tag-ulids '()) (ref user :id))))
      
      (if (getf result :success)
          (let* ((tags (getf result :tags))
                 (tags-json (mapcar #'tag-to-json-simple tags)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tags" . ,tags-json))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

(defmethod do-delete ((controller <todo-tags-controller>))
  "Remove a specific tag from a TODO."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (param controller "ulid"))
           (tag-ulid (param controller "tagUlid"))
           (result (remove-todo-tag ulid tag-ulid (ref user :id))))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("message" . "Tag removed successfully"))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))
