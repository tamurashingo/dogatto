; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/todo-tags-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/models/todo
                #:find-todo-by-ulid)
  (:import-from #:dogatto/models/todo-tag
                #:assign-tags-to-todo
                #:find-tags-for-todo)
  (:import-from #:dogatto/utils/session
                #:get-session
                #:session-valid-p)
  (:import-from #:dogatto/models/user
                #:find-user-by-id)
  (:import-from #:clails/model
                #:ref)
  (:import-from #:jonathan
                #:to-json)
  (:export #:<todo-tags-controller>))

(in-package #:dogatto/controllers/todo-tags-controller)

(defclass <todo-tags-controller> (<rest-controller>)
  ()
  (:documentation "Controller for managing TODO tags (GET /todos/:ulid/tags, PUT /todos/:ulid/tags)"))

(defun get-authenticated-user (env)
  "Get authenticated user from session.

   @param env [plist] Request environment
   @return [<user>] User instance if authenticated
   @return [nil] If not authenticated
   "
  (let* ((cookies (getf env :cookies))
         (session-id (cdr (assoc "session_id" cookies :test #'string=))))
    (when session-id
      (let ((session (get-session session-id)))
        (when (and session (session-valid-p session))
          (find-user-by-id (getf session :user-id)))))))

(defun tag-to-json (tag)
  "Convert tag instance to JSON-friendly alist.

   @param tag [<tag>] Tag instance
   @return [list] Alist representation of tag
   "
  (list (cons "id" (ref tag :id))
        (cons "ulid" (ref tag :ulid))
        (cons "name" (ref tag :name))
        (cons "color" (ref tag :color))))

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
           (todo (find-todo-by-ulid ulid (ref user :id))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      (let* ((tags (find-tags-for-todo ulid))
             (tags-json (mapcar #'tag-to-json tags)))
        (setf (slot-value controller 'clails/controller/base-controller:code) 200)
        (set-response controller
                     `(("status" . "success")
                       ("data" . (("tags" . ,tags-json)))))))))

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
           (todo (find-todo-by-ulid ulid (ref user :id))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      (let ((tag-ulids (param controller "tagUlids")))
        (handler-case
            (progn
              (assign-tags-to-todo ulid (or tag-ulids '()))
              (let* ((tags (find-tags-for-todo ulid))
                     (tags-json (mapcar #'tag-to-json tags)))
                (setf (slot-value controller 'clails/controller/base-controller:code) 200)
                (set-response controller
                             `(("status" . "success")
                               ("data" . (("tags" . ,tags-json)))))))
          (error (e)
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(format nil "Failed to assign tags: ~A" e))))))))))
