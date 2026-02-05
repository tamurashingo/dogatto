; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/tags-controller
  (:use #:cl
        #:rove
        #:clails/test)
  (:import-from #:dogatto/controllers/tags-controller
                #:<tags-list-controller>
                #:<tag-item-controller>)
  (:import-from #:dogatto/models/user
                #:create-user)
  (:import-from #:dogatto/models/tag
                #:create-tag)
  (:import-from #:dogatto/utils/session
                #:create-session)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:clails/controller/base-controller
                #:do-get
                #:do-post
                #:do-put
                #:do-delete))
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

(deftest-suite :controller test-tags-list-get
  (testing "GET /api/v1/tags returns tags for authenticated user"
    (let* ((user (create-user :email "taglistuser@example.com"
                              :password-hash "password123"
                              :username "Tag List User"
                              :ulid "01234567890123456789012350"))
           (tag1 (create-tag (ref user :id) "Work-TagsListGet" :color "#FF5733"))
           (tag2 (create-tag (ref user :id) "Personal-TagsListGet" :color "#33FF57"))
           (controller (setup-authenticated-controller '<tags-list-controller> user))
           (result (do-get controller))
           (data (cdr (assoc "data" result :test #'string=)))
           (tags (cdr (assoc "tags" data :test #'string=))))
      
      (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 200 OK")
      (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
          "Status should be success")
      (ok (>= (length tags) 2)
          "Should have at least 2 tags")
      (destroy tag1)
      (destroy tag2)
      (destroy user)))
  
  (testing "GET /api/v1/tags fails without authentication"
    (let* ((controller (make-instance '<tags-list-controller>))
           (headers (make-hash-table :test 'equal))
           (env (list :headers headers)))
      (setf (slot-value controller 'clails/controller/base-controller::env) env)
      (let ((result (do-get controller)))
        (ok (= 401 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 401 Unauthorized")
        (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
            "Status should be error")))))

(deftest-suite :controller test-tags-list-post
  (testing "POST /api/v1/tags creates a new tag"
    (let* ((user (create-user :email "tagcreateuser@example.com"
                              :password-hash "password123"
                              :username "Tag Create User"
                              :ulid "01234567890123456789012350"))
           (controller (setup-authenticated-controller 
                       '<tags-list-controller>
                       user
                       (list (cons "name" "Urgent-TagsListPost")
                             (cons "color" "#FF0000"))))
           (result (do-post controller))
           (data (cdr (assoc "data" result :test #'string=)))
           (tag-data (cdr (assoc "tag" data :test #'string=))))
      
      (ok (= 201 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 201 Created")
      (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
          "Status should be success")
      (ok (string= "Urgent-TagsListPost" (cdr (assoc "name" tag-data :test #'string=)))
          "Tag name should be 'Urgent-TagsListPost'")
      (ok (string= "#FF0000" (cdr (assoc "color" tag-data :test #'string=)))
          "Tag color should be '#FF0000'")
      (destroy user)))
  
  (testing "POST /api/v1/tags uses default color when not provided"
    (let* ((user (create-user :email "tagdefaultcolor@example.com"
                              :password-hash "password123"
                              :username "Default Color User"
                              :ulid "01234567890123456789012350"))
           (controller (setup-authenticated-controller 
                       '<tags-list-controller>
                       user
                       (list (cons "name" "NoColor-TagsListPost"))))
           (result (do-post controller))
           (data (cdr (assoc "data" result :test #'string=)))
           (tag-data (cdr (assoc "tag" data :test #'string=))))
      
      (ok (= 201 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 201 Created")
      (ok (string= "#3B82F6" (cdr (assoc "color" tag-data :test #'string=)))
          "Should use default color")
      (destroy user)))
  
  (testing "POST /api/v1/tags fails without name"
    (let* ((user (create-user :email "tagnoname@example.com"
                              :password-hash "password123"
                              :username "No Name User"
                              :ulid "01234567890123456789012350"))
           (controller (setup-authenticated-controller '<tags-list-controller> user))
           (result (do-post controller)))
      
      (ok (= 400 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 400 Bad Request")
      (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
          "Status should be error")
      (destroy user))))

(deftest-suite :controller test-tag-item-get
  (testing "GET /api/v1/tags/:ulid returns a specific tag with statistics"
    (let* ((user (create-user :email "taggetuser@example.com"
                              :password-hash "password123"
                              :username "Tag Get User"
                              :ulid "01234567890123456789012350"))
           (tag (create-tag (ref user :id) "TestTag-TagItemGet" :color "#123456"))
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
          "Should return the correct tag")
      (destroy tag)
      (destroy user)))
  
  (testing "GET /api/v1/tags/:ulid fails for non-existent tag"
    (let* ((user (create-user :email "tagnotfound@example.com"
                              :password-hash "password123"
                              :username "Not Found User"
                              :ulid "01234567890123456789012350"))
           (controller (setup-authenticated-controller 
                       '<tag-item-controller>
                       user
                       (list (cons "ulid" "01NONEXISTENT0000000000000"))))
           (result (do-get controller)))
      
      (ok (= 404 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 404 Not Found")
      (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
          "Status should be error")
      (destroy user))))

(deftest-suite :controller test-tag-item-put
  (testing "PUT /api/v1/tags/:ulid updates a tag"
    (let* ((user (create-user :email "tagupdateuser@example.com"
                              :password-hash "password123"
                              :username "Tag Update User"
                              :ulid "01234567890123456789012350"))
           (tag (create-tag (ref user :id) "OldName-TagItemPut" :color "#000000"))
           (tag-ulid (ref tag :ulid))
           (controller (setup-authenticated-controller 
                       '<tag-item-controller>
                       user
                       (list (cons "ulid" tag-ulid)
                             (cons "name" "NewName-TagItemPut")
                             (cons "color" "#FFFFFF"))))
           (result (do-put controller))
           (data (cdr (assoc "data" result :test #'string=)))
           (tag-data (cdr (assoc "tag" data :test #'string=))))
      
      (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 200 OK")
      (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
          "Status should be success")
      (ok (string= "NewName-TagItemPut" (cdr (assoc "name" tag-data :test #'string=)))
          "Tag name should be updated")
      (ok (string= "#FFFFFF" (cdr (assoc "color" tag-data :test #'string=)))
          "Tag color should be updated")
      (destroy tag)
      (destroy user))))

(deftest-suite :controller test-tag-item-delete
  (testing "DELETE /api/v1/tags/:ulid deletes a tag"
    (let* ((user (create-user :email "tagdeleteuser@example.com"
                              :password-hash "password123"
                              :username "Tag Delete User"
                              :ulid "01234567890123456789012350"))
           (tag (create-tag (ref user :id) "ToDelete-TagItemDelete" :color "#000000"))
           (tag-ulid (ref tag :ulid))
           (controller (setup-authenticated-controller 
                       '<tag-item-controller>
                       user
                       `(("ulid" . ,tag-ulid))))
           (result (do-delete controller)))
      
      (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 200 OK")
      (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
          "Status should be success")
      (destroy user))))
