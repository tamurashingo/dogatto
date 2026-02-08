; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/labels-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/models/label
                #:create-label
                #:find-labels-by-owner
                #:find-label-by-ulid
                #:update-label
                #:delete-label
                #:estimate-todo-count-by-tags
                #:search-labels-by-name
                #:get-label-stats)
  (:import-from #:dogatto/models/label-tag
                #:assign-tags-to-label
                #:find-tags-for-label
                #:find-labels-by-tag-name)
  (:import-from #:dogatto/utils/session
                #:get-session
                #:session-valid-p)
  (:import-from #:dogatto/models/user
                #:find-user-by-id)
  (:import-from #:clails/model
                #:ref
                #:has-error-p
                #:ref-error)
  (:import-from #:jonathan
                #:to-json)
  (:export #:<labels-list-controller>
           #:<label-item-controller>
           #:<label-estimate-controller>))

(in-package #:dogatto/controllers/labels-controller)

(defclass <labels-list-controller> (<rest-controller>)
  ()
  (:documentation "Controller for labels collection (GET /labels, POST /labels)"))

(defclass <label-item-controller> (<rest-controller>)
  ()
  (:documentation "Controller for single label item (GET /labels/:ulid, PUT /labels/:ulid, DELETE /labels/:ulid)"))

(defclass <label-estimate-controller> (<rest-controller>)
  ()
  (:documentation "Controller for TODO count estimation (GET /labels/estimate-todo-count)"))

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
        (cons "color" (ref tag :color))))

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
        (append base-json (list (cons "tags" (mapcar #'tag-to-json tags))))
        base-json)))

(defmethod do-get ((controller <labels-list-controller>))
  "Get all labels for the authenticated user with optional filtering."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((query-params (getf (env controller) :query-string))
           (params (when query-params
                     (quri:url-decode-params query-params)))
           (page (parse-integer (or (cdr (assoc "page" params :test #'string=)) "1")
                                :junk-allowed t))
           (per-page (parse-integer (or (cdr (assoc "per_page" params :test #'string=)) "20")
                                    :junk-allowed t))
           (sort (cdr (assoc "sort" params :test #'string=)))
           (order (cdr (assoc "order" params :test #'string=)))
           (filter (cdr (assoc "filter" params :test #'string=)))
           (search-mode (cdr (assoc "search_mode" params :test #'string=)))
           (q (cdr (assoc "q" params :test #'string=)))
           (owner-id (ref user :id))
           (labels (cond
                     ;; Search by label name
                     ((and q search-mode (string= search-mode "label_name"))
                      (search-labels-by-name owner-id q))
                     ;; Search by tag name
                     ((and q search-mode (string= search-mode "tag_name"))
                      (find-labels-by-tag-name owner-id q))
                     ;; Normal list with pagination
                     (t
                      (find-labels-by-owner owner-id
                                           :page page
                                           :per-page per-page
                                           :sort (when sort (intern (string-upcase sort) :keyword))
                                           :order (when order (intern (string-upcase order) :keyword))
                                           :filter (when filter (intern (string-upcase filter) :keyword))))))
           (labels-json (mapcar #'(lambda (label)
                                    (let* ((tags (find-tags-for-label (ref label :ulid) owner-id))
                                           (tag-count (length tags))
                                           (todo-count 0)) ; TODO: Calculate actual count
                                      (label-to-json label tag-count todo-count)))
                                labels))
           (stats (get-label-stats owner-id)))
      
      (setf (slot-value controller 'clails/controller/base-controller:code) 200)
      (set-response controller
                   `(("status" . "success")
                     ("data" . (("labels" . ,labels-json)
                               ("stats" . (("totalLabels" . ,(getf stats :total-labels))
                                          ("usedLabels" . ,(getf stats :used-labels))
                                          ("unusedLabels" . ,(getf stats :unused-labels)))))))))))

(defmethod do-post ((controller <labels-list-controller>))
  "Create a new label."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-post
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((body (getf (env controller) :raw-body))
           (json-data (jonathan:parse body :as :alist))
           (name (cdr (assoc "name" json-data :test #'string=)))
           (description (cdr (assoc "description" json-data :test #'string=)))
           (tag-ulids (cdr (assoc "tagUlids" json-data :test #'string=)))
           (owner-id (ref user :id)))
      
      ;; Validate input
      (unless name
        (setf (slot-value controller 'clails/controller/base-controller:code) 400)
        (return-from do-post
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Label name is required")))))
      
      (unless (and tag-ulids (listp tag-ulids) (> (length tag-ulids) 0))
        (setf (slot-value controller 'clails/controller/base-controller:code) 400)
        (return-from do-post
          (set-response controller
                       `(("status" . "error")
                         ("message" . "At least one tag is required")))))
      
      (handler-case
          (let ((label (create-label owner-id name description tag-ulids)))
            (if label
                (progn
                  ;; Assign tags to label
                  (assign-tags-to-label (ref label :ulid) tag-ulids owner-id)
                  
                  (let* ((tags (find-tags-for-label (ref label :ulid) owner-id))
                         (tag-count (length tags))
                         (todo-count 0))
                    (setf (slot-value controller 'clails/controller/base-controller:code) 201)
                    (set-response controller
                                 `(("status" . "success")
                                   ("data" . (("label" . ,(label-to-json label tag-count todo-count))))))))
                (progn
                  (setf (slot-value controller 'clails/controller/base-controller:code) 400)
                  (set-response controller
                               `(("status" . "error")
                                 ("message" . "Validation failed"))))))
        (error (e)
          (setf (slot-value controller 'clails/controller/base-controller:code) 400)
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,(format nil "~A" e)))))))))

(defmethod do-get ((controller <label-item-controller>))
  "Get a single label by ULID."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (getf (params controller) :ulid))
           (owner-id (ref user :id))
           (label (find-label-by-ulid ulid owner-id)))
      
      (unless label
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Label not found")))))
      
      (let* ((tags (find-tags-for-label ulid owner-id))
             (tag-count (length tags))
             (todo-count 0)) ; TODO: Calculate actual count
        (setf (slot-value controller 'clails/controller/base-controller:code) 200)
        (set-response controller
                     `(("status" . "success")
                       ("data" . (("label" . ,(label-to-json label tag-count todo-count tags))))))))))

(defmethod do-put ((controller <label-item-controller>))
  "Update a label."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (getf (params controller) :ulid))
           (body (getf (env controller) :raw-body))
           (json-data (jonathan:parse body :as :alist))
           (name (cdr (assoc "name" json-data :test #'string=)))
           (description (cdr (assoc "description" json-data :test #'string=)))
           (tag-ulids (cdr (assoc "tagUlids" json-data :test #'string=)))
           (owner-id (ref user :id)))
      
      (handler-case
          (let ((label (update-label ulid owner-id
                                    :name name
                                    :description description
                                    :tag-ulids tag-ulids)))
            (unless label
              (setf (slot-value controller 'clails/controller/base-controller:code) 404)
              (return-from do-put
                (set-response controller
                             `(("status" . "error")
                               ("message" . "Label not found")))))
            
            ;; Update tags if provided
            (when tag-ulids
              (assign-tags-to-label ulid tag-ulids owner-id))
            
            (let* ((tags (find-tags-for-label ulid owner-id))
                   (tag-count (length tags))
                   (todo-count 0))
              (setf (slot-value controller 'clails/controller/base-controller:code) 200)
              (set-response controller
                           `(("status" . "success")
                             ("data" . (("label" . ,(label-to-json label tag-count todo-count))))))))
        (error (e)
          (setf (slot-value controller 'clails/controller/base-controller:code) 400)
          (set-response controller
                       `(("status" . "error")
                         ("message" . ,(format nil "~A" e)))))))))

(defmethod do-delete ((controller <label-item-controller>))
  "Delete a label."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((ulid (getf (params controller) :ulid))
           (owner-id (ref user :id))
           (result (delete-label ulid owner-id)))
      
      (unless result
        (setf (slot-value controller 'clails/controller/base-controller:code) 404)
        (return-from do-delete
          (set-response controller
                       `(("status" . "error")
                         ("message" . "Label not found")))))
      
      (setf (slot-value controller 'clails/controller/base-controller:code) 204)
      (set-response controller nil))))

(defmethod do-get ((controller <label-estimate-controller>))
  "Estimate TODO count for given tag ULIDs."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    (let* ((query-params (getf (env controller) :query-string))
           (params (when query-params
                     (quri:url-decode-params query-params)))
           (tag-ulids-str (cdr (assoc "tag_ulids" params :test #'string=)))
           (tag-ulids (when tag-ulids-str
                        (cl-ppcre:split "," tag-ulids-str)))
           (owner-id (ref user :id)))
      
      (unless tag-ulids
        (setf (slot-value controller 'clails/controller/base-controller:code) 400)
        (return-from do-get
          (set-response controller
                       `(("status" . "error")
                         ("message" . "tag_ulids parameter is required")))))
      
      (let ((count (estimate-todo-count-by-tags owner-id tag-ulids)))
        (setf (slot-value controller 'clails/controller/base-controller:code) 200)
        (set-response controller
                     `(("status" . "success")
                       ("data" . (("count" . ,count)))))))))
