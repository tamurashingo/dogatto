; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/tags-controller
  (:use #:cl
        #:rove
        #:clails/test)
  (:import-from #:dogatto/controllers/tags-controller
                #:<tags-list-controller>
                #:<tag-item-controller>
                #:do-get
                #:do-post
                #:do-put
                #:do-delete)
  (:import-from #:dogatto/models/user
                #:create-user
                #:<user>)
  (:import-from #:dogatto/models/tag
                #:create-tag
                #:<tag>)
  (:import-from #:dogatto/utils/session
                #:create-session
                #:delete-session)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:clails/controller/base-controller
                #:response))
(in-package #:dogatto-test/controllers/tags-controller)

(defun setup-authenticated-controller (controller-class user &optional params)
  "Setup controller with authentication session.

   @param controller-class [symbol] Controller class to instantiate
   @param user [<user>] User instance for authentication
   @param params [alist] Optional parameters to set (optional)
   @return [<base-controller>] Initialized controller instance
   "
  (let* ((session-id (create-session (ref user :id)))
         (controller (make-instance controller-class))
         (headers (make-hash-table :test 'equal))
         (env (list :headers headers
                   :cookies (list (cons "session_id" session-id))
                   :current-user user)))
    ;; Set Cookie header for authentication
    (setf (gethash "cookie" headers) (format nil "session_id=~A" session-id))
    (setf (slot-value controller 'clails/controller/base-controller::env) env)
    (when params
      (loop for (key . value) in params
            do (setf (gethash key (slot-value controller 'clails/controller/base-controller::params))
                     value)))
    controller))

(deftest test-tags-list-get
  (testing "GET /api/v1/tags returns tags for authenticated user"
    (with-transaction
      (let* ((user (create-user "taglistuser@example.com" "password123" "Tag List User"))
             (tag1 (create-tag (ref user :id) "Work" :color "#FF5733"))
             (tag2 (create-tag (ref user :id) "Personal" :color "#33FF57"))
             (controller (setup-authenticated-controller '<tags-list-controller> user))
             (result (do-get controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (tags (cdr (assoc "tags" data :test #'string=))))
        
        (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 200 OK")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok (>= (length tags) 2)
            "Should have at least 2 tags"))))
  
  (testing "GET /api/v1/tags fails without authentication"
    (let* ((controller (make-instance '<tags-list-controller>))
           (env (list :cookies '())))
      (setf (slot-value controller 'clails/controller/base-controller::env) env)
      (let ((result (do-get controller)))
        (ok (= 401 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 401 Unauthorized")
        (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
            "Status should be error")))))

(deftest test-tags-list-post
  (testing "POST /api/v1/tags creates a new tag"
    (with-transaction
      (let* ((user (create-user "tagcreateuser@example.com" "password123" "Tag Create User"))
             (controller (setup-authenticated-controller 
                         '<tags-list-controller>
                         user
                         (list (cons "name" "Urgent")
                               (cons "color" "#FF0000"))))
             (result (do-post controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (tag (cdr (assoc "tag" data :test #'string=))))
        
        (ok (= 201 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 201 Created")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok (string= "Urgent" (cdr (assoc "name" tag :test #'string=)))
            "Tag name should be 'Urgent'")
        (ok (string= "#FF0000" (cdr (assoc "color" tag :test #'string=)))
            "Tag color should be '#FF0000'"))))
  
  (testing "POST /api/v1/tags uses default color when not provided"
    (with-transaction
      (let* ((user (create-user "tagdefaultcolor@example.com" "password123" "Default Color User"))
             (controller (setup-authenticated-controller 
                         '<tags-list-controller>
                         user
                         (list (cons "name" "NoColor"))))
             (result (do-post controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (tag (cdr (assoc "tag" data :test #'string=))))
        
        (ok (= 201 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 201 Created")
        (ok (string= "#3B82F6" (cdr (assoc "color" tag :test #'string=)))
            "Should use default color"))))
  
  (testing "POST /api/v1/tags fails without name"
    (with-transaction
      (let* ((user (create-user "tagnoname@example.com" "password123" "No Name User"))
             (controller (setup-authenticated-controller '<tags-list-controller> user))
             (result (do-post controller)))
        
        (ok (= 400 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 400 Bad Request")
        (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
            "Status should be error")))))

(deftest test-tag-item-get
  (testing "GET /api/v1/tags/:ulid returns a specific tag with statistics"
    (with-transaction
      (let* ((user (create-user "taggetuser@example.com" "password123" "Tag Get User"))
             (tag (create-tag (ref user :id) "TestTag" :color "#123456"))
             (tag-ulid (ref tag :ulid))
             (controller (setup-authenticated-controller 
                         '<tag-item-controller>
                         user
                         (list (cons "ulid" tag-ulid))))
             (result (do-get controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (tag-data (cdr (assoc "tag" data :test #'string=))))
        
        (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 200 OK")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok (string= tag-ulid (cdr (assoc "ulid" tag-data :test #'string=)))
            "Should return the correct tag"))))
  
  (testing "GET /api/v1/tags/:ulid fails for non-existent tag"
    (with-transaction
      (let* ((user (create-user "tagnotfound@example.com" "password123" "Not Found User"))
             (controller (setup-authenticated-controller 
                         '<tag-item-controller>
                         user
                         (list (cons "ulid" "01NONEXISTENT0000000000000"))))
             (result (do-get controller)))
        
        (ok (= 404 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 404 Not Found")
        (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
            "Status should be error")))))

(deftest test-tag-item-put
  (testing "PUT /api/v1/tags/:ulid updates a tag"
    (with-transaction
      (let* ((user (create-user "tagupdateuser@example.com" "password123" "Tag Update User"))
             (tag (create-tag (ref user :id) "OldName" :color "#000000"))
             (tag-ulid (ref tag :ulid))
             (controller (setup-authenticated-controller 
                         '<tag-item-controller>
                         user
                         (list (cons "ulid" tag-ulid)
                               (cons "name" "NewName")
                               (cons "color" "#FFFFFF"))))
             (result (do-put controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (tag-data (cdr (assoc "tag" data :test #'string=))))
        
        (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 200 OK")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok (string= "NewName" (cdr (assoc "name" tag-data :test #'string=)))
            "Tag name should be updated")
        (ok (string= "#FFFFFF" (cdr (assoc "color" tag-data :test #'string=)))
            "Tag color should be updated")))))

(deftest test-tag-item-delete
  (testing "DELETE /api/v1/tags/:ulid deletes a tag"
    (with-transaction
      (let* ((user (create-user "tagdeleteuser@example.com" "password123" "Tag Delete User"))
             (tag (create-tag (ref user :id) "ToDelete" :color "#000000"))
             (tag-ulid (ref tag :ulid))
             (controller (setup-authenticated-controller 
                         '<tag-item-controller>
                         user
                         (list (cons "ulid" tag-ulid))))
             (result (do-delete controller)))
        
        (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 200 OK")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")))))
