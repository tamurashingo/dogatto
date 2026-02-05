; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/tags-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/models/tag
                #:create-tag
                #:find-tags-by-user
                #:find-tag-by-ulid
                #:update-tag
                #:delete-tag)
  (:import-from #:dogatto/models/todo-tag
                #:get-tag-statistics)
  (:import-from #:dogatto/utils/session
                #:get-session
                #:session-valid-p)
  (:import-from #:dogatto/models/user
                #:find-user-by-id)
  (:import-from #:clails/model
                #:ref)
  (:import-from #:jonathan
                #:to-json)
  (:export #:<tags-list-controller>
           #:<tag-item-controller>))

(in-package #:dogatto/controllers/tags-controller)

(defclass <tags-list-controller> (<rest-controller>)
  ()
  (:documentation "Controller for tags collection (GET /tags, POST /tags)"))

(defclass <tag-item-controller> (<rest-controller>)
  ()
  (:documentation "Controller for single tag item (GET /tags/:ulid, PUT /tags/:ulid, DELETE /tags/:ulid)"))

(defun get-cookie-value (headers cookie-name)
  "Extract cookie value from request headers.

   @param headers [hash-table] Request headers
   @param cookie-name [string] Name of the cookie to extract
   @return [string] Cookie value if found
   @return [nil] If cookie not found
   "
  (let ((cookie-header (gethash "cookie" headers)))
    (when cookie-header
      (let* ((cookies (cl-ppcre:split ";\\s*" cookie-header))
             (target-cookie (find-if (lambda (c)
                                       (cl-ppcre:scan (format nil "^~A=" cookie-name) c))
                                     cookies)))
        (when target-cookie
          (cadr (cl-ppcre:split "=" target-cookie :limit 2)))))))

(defun get-authenticated-user (env)
  "Get authenticated user from session.

   Extracts session ID from cookies, validates it, and returns the user.

   @param env [plist] Request environment
   @return [<user>] Authenticated user
   @return [nil] If not authenticated
   "
  (let* ((headers (getf env :headers))
         (session-id (get-cookie-value headers "session_id")))
    (when (and session-id (session-valid-p session-id))
      (let* ((session-data (get-session session-id))
             (user-id (getf session-data :user-id)))
        (when user-id
          (find-user-by-id user-id))))))

(defun tag-to-json (tag)
  "Convert tag instance to JSON-friendly alist.

   @param tag [<tag>] Tag instance
   @return [list] Alist representation of tag
   "
  (list (cons "id" (ref tag :id))
        (cons "ulid" (ref tag :ulid))
        (cons "name" (ref tag :name))
        (cons "color" (ref tag :color))
        (cons "createdAt" (ref tag :created-at))
        (cons "updatedAt" (ref tag :updated-at))))

(defmethod do-get ((controller <tags-list-controller>))
  "Get all tags for the authenticated user."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((tags (find-tags-by-user (ref user :id)))
           (tags-json (mapcar #'tag-to-json tags)))
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("data" . (("tags" . ,tags-json))))))))

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
           (color (or (param controller "color") "#3B82F6")))
      
      (unless name
        (setf (slot-value controller 'clails/controller/base-controller:code) 400)
        (return-from do-post
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Tag name is required")))))
      
      (handler-case
          (let ((tag (create-tag (ref user :id) name :color color)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 201)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tag" . ,(tag-to-json tag)))))))
        (error (e)
          (setf (slot-value controller 'clails/controller/base-controller:code) 400)
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,(format nil "Failed to create tag: ~A" e)))))))))

(defmethod do-get ((controller <tag-item-controller>))
  "Get a specific tag with statistics."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (param controller "ulid"))
           (tag (find-tag-by-ulid ulid (ref user :id))))
      
      (unless tag
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Tag not found")))))
      
      (let ((stats (get-tag-statistics ulid (ref user :id))))
        (setf (slot-value controller 'clails/controller/base-controller:code) 200)
        (set-response controller
                     `(("status" . "success")
                       ("data" . (("tag" . ,(tag-to-json tag))
                                  ("statistics" . ,stats)))))))))

(defmethod do-put ((controller <tag-item-controller>))
  "Update a tag."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (param controller "ulid"))
           (tag (find-tag-by-ulid ulid (ref user :id))))
      
      (unless tag
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Tag not found")))))
      
      (handler-case
          (progn
            (let ((name (param controller "name"))
                  (color (param controller "color")))
              (when name
                (update-tag tag :name name))
              (when color
                (update-tag tag :color color)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("tag" . ,(tag-to-json tag)))))))
        (error (e)
          (setf (slot-value controller 'clails/controller/base-controller:code) 400)
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,(format nil "Failed to update tag: ~A" e)))))))))

(defmethod do-delete ((controller <tag-item-controller>))
  "Delete a tag."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (param controller "ulid"))
           (tag (find-tag-by-ulid ulid (ref user :id))))
      
      (unless tag
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-delete
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Tag not found")))))
      
      (delete-tag tag)
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("message" . "Tag deleted successfully"))))))
