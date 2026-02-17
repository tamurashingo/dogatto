; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/tags-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/services/tag-service
                #:list-tags
                #:get-tag-with-stats
                #:create-new-tag
                #:update-existing-tag
                #:delete-existing-tag)
  (:import-from #:dogatto/helpers/auth-helper
                #:get-authenticated-user)
  (:import-from #:dogatto/helpers/json-converters
                #:tag-to-json)
  (:import-from #:clails/model
                #:ref)
  (:export #:<tags-list-controller>
           #:<tag-item-controller>))

(in-package #:dogatto/controllers/tags-controller)

(defclass <tags-list-controller> (<rest-controller>)
  ()
  (:documentation "Controller for tags collection (GET /tags, POST /tags)"))

(defclass <tag-item-controller> (<rest-controller>)
  ()
  (:documentation "Controller for single tag item (GET /tags/:ulid, PUT /tags/:ulid, DELETE /tags/:ulid)"))

;; GET /api/v1/tags
(defmethod do-get ((controller <tags-list-controller>))
  "Get all tags for the authenticated user."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((exclude-merged (string= (param controller "excludeMerged") "true"))
           (result (list-tags (ref user :id) :exclude-merged exclude-merged)))
      
      (if (getf result :success)
          (let* ((tags (getf result :tags))
                 (tags-json (mapcar #'tag-to-json tags)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tags" . ,tags-json))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 500)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; POST /api/v1/tags
(defmethod do-post ((controller <tags-list-controller>))
  "Create a new tag for the authenticated user."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-post
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((name (param controller "name"))
           (color (param controller "color"))
           (result (create-new-tag (ref user :id) name :color color)))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 201)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tag" . ,(tag-to-json (getf result :tag))))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; GET /api/v1/tags/:ulid
(defmethod do-get ((controller <tag-item-controller>))
  "Get a specific tag by ULID with statistics."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((tag-ulid (param controller "ulid"))
           (result (get-tag-with-stats tag-ulid (ref user :id))))
      
      (if (getf result :success)
          (let* ((tag (getf result :tag))
                 (stats (getf result :statistics))
                 (tag-json (tag-to-json tag))
                 (response-data (append tag-json
                                       `(("activeTodoCount" . ,(getf stats :active-count))
                                         ("completedTodoCount" . ,(getf stats :completed-count))))))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tag" . ,response-data))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; PUT /api/v1/tags/:ulid
(defmethod do-put ((controller <tag-item-controller>))
  "Update an existing tag."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((tag-ulid (param controller "ulid"))
           (name (param controller "name"))
           (color (param controller "color"))
           (result (update-existing-tag tag-ulid (ref user :id)
                                       :name name
                                       :color color)))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tag" . ,(tag-to-json (getf result :tag))))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; DELETE /api/v1/tags/:ulid
(defmethod do-delete ((controller <tag-item-controller>))
  "Delete a tag."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((tag-ulid (param controller "ulid"))
           (result (delete-existing-tag tag-ulid (ref user :id))))
      
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("message" . "Tag deleted successfully"))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))
