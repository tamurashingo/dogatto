; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/tags-merge-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/services/tag-merge-service
                #:merge-tags-to-existing
                #:merge-tags-to-new)
  (:import-from #:dogatto/utils/session
                #:get-session
                #:session-valid-p)
  (:import-from #:dogatto/utils/request
                #:read-body-as-string)
  (:import-from #:dogatto/models/user
                #:find-user-by-id)
  (:import-from #:dogatto/models/todo-tag
                #:get-tag-statistics)
  (:import-from #:clails/model
                #:ref)
  (:import-from #:jonathan
                #:to-json
                #:parse)
  (:export #:<tags-merge-controller>
           #:<tags-merge-to-new-controller>))

(in-package #:dogatto/controllers/tags-merge-controller)

(defclass <tags-merge-controller> (<rest-controller>)
  ()
  (:documentation "Controller for merging tags to existing tag (POST /api/v1/tags/merge)"))

(defclass <tags-merge-to-new-controller> (<rest-controller>)
  ()
  (:documentation "Controller for merging tags to new tag (POST /api/v1/tags/merge-to-new)"))

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
        (cons "updatedAt" (ref tag :updated-at))
        (cons "mergedToUlid" (ref tag :merged-to-ulid))
        (cons "mergedAt" (ref tag :merged-at))))

(defun tag-to-json-with-stats (tag)
  "Convert tag instance to JSON with statistics.

   @param tag [<tag>] Tag instance
   @return [list] Alist representation with stats
   "
  (let* ((stats (get-tag-statistics (ref tag :id)))
         (basic (tag-to-json tag)))
    (append basic
            (list (cons "todoCount" (getf stats :total 0))
                  (cons "completedCount" (getf stats :completed 0))
                  (cons "activeCount" (getf stats :active 0))))))

(defun merged-tag-to-json (tag target-tag)
  "Convert merged tag to JSON response format.

   @param tag [<tag>] Source tag that was merged
   @param target-tag [<tag>] Target tag
   @return [list] Alist representation
   "
  (list (cons "ulid" (ref tag :ulid))
        (cons "name" (ref tag :name))
        (cons "mergedTo" (list (cons "ulid" (ref target-tag :ulid))
                               (cons "name" (ref target-tag :name))))
        (cons "mergedAt" (ref tag :merged-at))))

(defun error-response (status message &optional details)
  "Create error response.

   @param status [integer] HTTP status code
   @param message [string] Error message
   @param details [list] Optional error details
   @return [list] HTTP response list
   "
  (list status
        '(:content-type "application/json")
        (list (to-json
               (if details
                   (list (cons "status" "error")
                         (cons "message" message)
                         (cons "details" details))
                   (list (cons "status" "error")
                         (cons "message" message)))))))

(defun success-response (data &optional (status 200))
  "Create success response.

   @param data [list] Response data
   @param status [integer] HTTP status code (default 200)
   @return [list] HTTP response list
   "
  (list status
        '(:content-type "application/json")
        (list (to-json
               (list (cons "status" "success")
                     (cons "data" data))))))

;;; POST /api/v1/tags/merge - Merge tags to existing tag
(defmethod do-post ((controller <tags-merge-controller>))
  (let* ((env (ref controller :env))
         (user (get-authenticated-user env)))
    
    ;; Check authentication
    (unless user
      (return-from do-post
        (error-response 401 "Authentication required")))
    
    (handler-case
        (let* ((body (read-body-as-string env))
               (json-data (parse body :as :alist))
               (source-ulids (cdr (assoc "source_ulids" json-data :test #'string=)))
               (target-ulid (cdr (assoc "target_ulid" json-data :test #'string=)))
               (owner-id (ref user :id)))
          
          ;; Validate input
          (unless source-ulids
            (return-from do-post
              (error-response 400 "source_ulids is required")))
          
          (unless target-ulid
            (return-from do-post
              (error-response 400 "target_ulid is required")))
          
          ;; Execute merge
          (let ((result (merge-tags-to-existing source-ulids target-ulid owner-id)))
            (if (getf result :success)
                ;; Success
                (let* ((merged-tags (getf result :merged-tags))
                       (target-tag (getf result :target-tag))
                       (merged-tags-json (mapcar (lambda (tag)
                                                   (merged-tag-to-json tag target-tag))
                                                 merged-tags))
                       (target-tag-json (tag-to-json-with-stats target-tag)))
                  (success-response
                   (list (cons "mergedTags" merged-tags-json)
                         (cons "targetTag" target-tag-json))))
                ;; Failure
                (let ((errors (getf result :errors)))
                  (error-response 400 "Merge failed"
                                  (list (cons "errors" errors)))))))
      
      (error (e)
        (error-response 500 (format nil "Internal server error: ~A" e))))))

;;; POST /api/v1/tags/merge-to-new - Merge tags to new tag
(defmethod do-post ((controller <tags-merge-to-new-controller>))
  (let* ((env (ref controller :env))
         (user (get-authenticated-user env)))
    
    ;; Check authentication
    (unless user
      (return-from do-post
        (error-response 401 "Authentication required")))
    
    (handler-case
        (let* ((body (read-body-as-string env))
               (json-data (parse body :as :alist))
               (source-ulids (cdr (assoc "source_ulids" json-data :test #'string=)))
               (new-tag-data (cdr (assoc "new_tag" json-data :test #'string=)))
               (new-tag-name (cdr (assoc "name" new-tag-data :test #'string=)))
               (new-tag-color (or (cdr (assoc "color" new-tag-data :test #'string=))
                                  "#3B82F6"))
               (owner-id (ref user :id)))
          
          ;; Validate input
          (unless source-ulids
            (return-from do-post
              (error-response 400 "source_ulids is required")))
          
          (unless new-tag-name
            (return-from do-post
              (error-response 400 "new_tag.name is required")))
          
          ;; Execute merge
          (let ((result (merge-tags-to-new source-ulids new-tag-name owner-id
                                           :color new-tag-color)))
            (if (getf result :success)
                ;; Success
                (let* ((merged-tags (getf result :merged-tags))
                       (new-tag (getf result :new-tag))
                       (merged-tags-json (mapcar (lambda (tag)
                                                   (merged-tag-to-json tag new-tag))
                                                 merged-tags))
                       (new-tag-json (tag-to-json-with-stats new-tag)))
                  (success-response
                   (list (cons "mergedTags" merged-tags-json)
                         (cons "newTag" new-tag-json))
                   201))
                ;; Failure
                (let ((errors (getf result :errors)))
                  (error-response 400 "Merge failed"
                                  (list (cons "errors" errors)))))))
      
      (error (e)
        (error-response 500 (format nil "Internal server error: ~A" e))))))
