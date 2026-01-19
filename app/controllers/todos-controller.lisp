; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/todos-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/models/todo
                #:create-todo
                #:find-todo-by-id
                #:find-todo-by-ulid
                #:find-todos-by-user
                #:update-todo
                #:delete-todo
                #:toggle-todo-status
                #:<todo>)
  (:import-from #:dogatto/models/user
                #:find-user-by-id)
  (:import-from #:dogatto/utils/session
                #:get-session
                #:session-valid-p)
  (:import-from #:dogatto/utils/time-conversion
                #:universal-time-to-unix-time
                #:unix-time-to-universal-time)
  (:import-from #:clails/model
                #:ref)
  (:import-from #:jonathan
                #:to-json)
  (:export #:<todos-list-controller>
           #:<todo-item-controller>
           #:<todo-complete-controller>))

(in-package #:dogatto/controllers/todos-controller)

(defclass <todos-list-controller> (<rest-controller>)
  ()
  (:documentation "Controller for todos collection (GET /todos, POST /todos)"))

(defclass <todo-item-controller> (<rest-controller>)
  ()
  (:documentation "Controller for single todo item (GET /todos/:id, PUT /todos/:id, DELETE /todos/:id)"))

(defclass <todo-complete-controller> (<rest-controller>)
  ()
  (:documentation "Controller for completing a todo (POST /todos/:id/complete)"))

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

(defun todo-to-json (todo)
  "Convert todo model to JSON-safe alist.

   @param todo [<todo>] TODO model instance
   @return [list] Alist representation of TODO
   "
  (let ((content-val (ref todo :content))
        (completed-at-val (ref todo :completed-at))
        (due-date-val (ref todo :due-date)))
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
          (cons "updatedAt" (universal-time-to-unix-time (ref todo :updated-at))))))

(defmethod do-get ((controller <todos-list-controller>))
  "Get all todos for the authenticated user.

   Returns JSON array of todos.

   @param controller [<todos-list-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((user-id (ref user :id))
           (todos (find-todos-by-user user-id))
           (todos-json (mapcar #'todo-to-json todos)))
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("data" . (("todos" . ,todos-json))))))))

(defmethod do-post ((controller <todos-list-controller>))
  "Create a new todo for the authenticated user.

   Expects JSON body with: title (required), content (optional), dueDate (optional)

   @param controller [<todos-list-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-post
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((title (param controller "title"))
           (content (param controller "content"))
           (due-date-raw (param controller "dueDate"))
           (due-date (cond
                       ((null due-date-raw) nil)
                       ((numberp due-date-raw) (unix-time-to-universal-time due-date-raw))
                       ((stringp due-date-raw) 
                        (let ((unix-time (parse-integer due-date-raw :junk-allowed t)))
                          (when unix-time (unix-time-to-universal-time unix-time))))
                       (t nil))))
      
      (unless title
        (setf (slot-value controller 'clails/controller/base-controller:code) 400)
        (return-from do-post
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Title is required")))))
      
      (handler-case
          (let ((todo (create-todo (ref user :id) title
                                  :content content
                                  :due-date due-date)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 201)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todo" . ,(todo-to-json todo)))))))
        (simple-error (e)
          (setf (slot-value controller 'clails/controller/base-controller:code) 400)
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,(format nil "~A" e)))))))))

(defmethod do-get ((controller <todo-item-controller>))
  "Get a specific todo by ULID.

   Only returns the todo if it belongs to the authenticated user.

   @param controller [<todo-item-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (todo (when todo-ulid (find-todo-by-ulid todo-ulid))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      ;; Check if todo belongs to current user
      (unless (= (ref todo :owner-id) (ref user :id))
        (setf (slot-value controller 'clails/controller/base-controller:code) 403)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Access denied")))))
      
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("data" . (("todo" . ,(todo-to-json todo)))))))))

(defmethod do-put ((controller <todo-item-controller>))
  "Update an existing todo.

   Only updates if todo belongs to authenticated user.
   Expects JSON body with: title (optional), content (optional), dueDate (optional)

   @param controller [<todo-item-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (todo (when todo-ulid (find-todo-by-ulid todo-ulid))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      ;; Check if todo belongs to current user
      (unless (= (ref todo :owner-id) (ref user :id))
        (setf (slot-value controller 'clails/controller/base-controller:code) 403)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Access denied")))))
      
      (let* ((title (param controller "title"))
             (content (param controller "content"))
             (due-date-raw (param controller "dueDate"))
             (due-date (cond
                         ((null due-date-raw) nil)
                         ((numberp due-date-raw) (unix-time-to-universal-time due-date-raw))
                         ((stringp due-date-raw)
                          (let ((unix-time (parse-integer due-date-raw :junk-allowed t)))
                            (when unix-time (unix-time-to-universal-time unix-time))))
                         (t nil))))
        
        (handler-case
            (progn
              (update-todo todo
                          :title title
                          :content content
                          :due-date due-date)
              (setf (slot-value controller 'clails/controller/base-controller:code) 200)
              (set-response controller
                           `(("status" . "success")
                             ("data" . (("todo" . ,(todo-to-json todo)))))))
          (simple-error (e)
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(format nil "~A" e))))))))))

(defmethod do-delete ((controller <todo-item-controller>))
  "Delete a todo.

   Only deletes if todo belongs to authenticated user.

   @param controller [<todo-item-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (todo (when todo-ulid (find-todo-by-ulid todo-ulid))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-delete
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      ;; Check if todo belongs to current user
      (unless (= (ref todo :owner-id) (ref user :id))
        (setf (slot-value controller 'clails/controller/base-controller:code) 403)
        (return-from do-delete
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Access denied")))))
      
      (delete-todo todo)
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("message" . "TODO deleted successfully"))))))

(defmethod do-put ((controller <todo-complete-controller>))
  "Mark a todo as completed.

   Only completes if todo belongs to authenticated user.
   Toggles the status between 'pending' and 'completed'.

   @param controller [<todo-complete-controller>] Controller instance
   @return [list] HTTP response via set-response
   "
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((todo-ulid (param controller "id"))
           (todo (when todo-ulid (find-todo-by-ulid todo-ulid))))
      
      (unless todo
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "TODO not found")))))
      
      ;; Check if todo belongs to current user
      (unless (= (ref todo :owner-id) (ref user :id))
        (setf (slot-value controller 'clails/controller/base-controller:code) 403)
        (return-from do-put
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Access denied")))))
      
      (toggle-todo-status todo)
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("data" . (("todo" . ,(todo-to-json todo)))))))))
