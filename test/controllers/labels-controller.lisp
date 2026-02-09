; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/labels-controller
  (:use #:cl
        #:rove
        #:clails/test)
  (:import-from #:dogatto/controllers/labels-controller
                #:<labels-list-controller>
                #:<label-item-controller>
                #:<label-estimate-controller>)
  (:import-from #:dogatto/models/user
                #:create-user)
  (:import-from #:dogatto/models/tag
                #:create-tag)
  (:import-from #:dogatto/models/label
                #:create-label)
  (:import-from #:dogatto/models/label-tag
                #:assign-tags-to-label)
  (:import-from #:dogatto/utils/session
                #:create-session)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:clails/controller/base-controller
                #:do-get
                #:do-post
                #:do-put
                #:do-delete)
  (:import-from #:jonathan
                #:to-json))
(in-package #:dogatto-test/controllers/labels-controller)

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

(deftest-suite :controller test-labels-list-get
  (testing "GET /api/v1/labels returns labels for authenticated user"
    (let* ((user (create-user :email "labellistuser@example.com"
                              :password-hash "password123"
                              :username "Label List User"
                              :ulid "01234567890123456789012360"))
           (tag1 (create-tag (ref user :id) "Work-LabelListGet" :color "#FF5733"))
           (tag2 (create-tag (ref user :id) "Personal-LabelListGet" :color "#33FF57"))
           (label1 (create-label (ref user :id) "Project A" "Test project" 
                                (list (ref tag1 :ulid))))
           _ (assign-tags-to-label (ref label1 :ulid) (list (ref tag1 :ulid)) (ref user :id))
           (controller (setup-authenticated-controller '<labels-list-controller> user))
           (result (do-get controller))
           (data (cdr (assoc "data" result :test #'string=)))
           (labels (cdr (assoc "labels" data :test #'string=)))
           (stats (cdr (assoc "stats" data :test #'string=))))
      
      (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 200 OK")
      (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
          "Status should be success")
      (ok (>= (length labels) 1)
          "Should have at least 1 label")
      (ok stats "Should include stats")
      
      (destroy label1)
      (destroy tag1)
      (destroy tag2)
      (destroy user)))
  
  (testing "GET /api/v1/labels fails without authentication"
    (let* ((controller (make-instance '<labels-list-controller>))
           (headers (make-hash-table :test 'equal))
           (env (list :headers headers)))
      (setf (slot-value controller 'clails/controller/base-controller::env) env)
      (let ((result (do-get controller)))
        (ok (= 401 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 401 Unauthorized")
        (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
            "Status should be error")))))

(deftest-suite :controller test-labels-post
  (testing "POST /api/v1/labels creates new label"
    (let* ((user (create-user :email "labelcreateuser@example.com"
                              :password-hash "password123"
                              :username "Label Create User"
                              :ulid "01234567890123456789012361"))
           (tag1 (create-tag (ref user :id) "Work-LabelCreate" :color "#FF5733"))
           (controller (setup-authenticated-controller '<labels-list-controller> user))
           (body (jonathan:to-json 
                  `(("name" . "New Label")
                    ("description" . "Test description")
                    ("tagUlids" . (,(ref tag1 :ulid))))))
           (env (slot-value controller 'clails/controller/base-controller::env)))
      
      (setf (getf env :raw-body) body)
      (let* ((result (do-post controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (label (cdr (assoc "label" data :test #'string=))))
        
        (ok (= 201 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 201 Created")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok label "Should return created label")
        (ok (string= "New Label" (cdr (assoc "name" label :test #'string=)))
            "Label name should match"))
      
      (destroy tag1)
      (destroy user)))
  
  (testing "POST /api/v1/labels fails without tag ULIDs"
    (let* ((user (create-user :email "labelfailuser@example.com"
                              :password-hash "password123"
                              :username "Label Fail User"
                              :ulid "01234567890123456789012362"))
           (controller (setup-authenticated-controller '<labels-list-controller> user))
           (body (jonathan:to-json 
                  `(("name" . "No Tags Label")
                    ("description" . "Should fail"))))
           (env (slot-value controller 'clails/controller/base-controller::env)))
      
      (setf (getf env :raw-body) body)
      (let ((result (do-post controller)))
        (ok (= 400 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 400 Bad Request")
        (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
            "Status should be error"))
      
      (destroy user))))

(deftest-suite :controller test-label-item-get
  (testing "GET /api/v1/labels/:ulid returns label details"
    (let* ((user (create-user :email "labelgetuser@example.com"
                              :password-hash "password123"
                              :username "Label Get User"
                              :ulid "01234567890123456789012363"))
           (tag1 (create-tag (ref user :id) "Work-LabelGet" :color "#FF5733"))
           (label1 (create-label (ref user :id) "Project B" "Test project B" 
                                (list (ref tag1 :ulid))))
           _ (assign-tags-to-label (ref label1 :ulid) (list (ref tag1 :ulid)) (ref user :id))
           (params `((:ulid . ,(ref label1 :ulid))))
           (controller (setup-authenticated-controller '<label-item-controller> user params))
           (result (do-get controller))
           (data (cdr (assoc "data" result :test #'string=)))
           (label (cdr (assoc "label" data :test #'string=))))
      
      (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 200 OK")
      (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
          "Status should be success")
      (ok (string= "Project B" (cdr (assoc "name" label :test #'string=)))
          "Label name should match")
      
      (destroy label1)
      (destroy tag1)
      (destroy user)))
  
  (testing "GET /api/v1/labels/:ulid returns 404 for non-existent label"
    (let* ((user (create-user :email "label404user@example.com"
                              :password-hash "password123"
                              :username "Label 404 User"
                              :ulid "01234567890123456789012364"))
           (params `((:ulid . "01NONEXISTENT0000000000000")))
           (controller (setup-authenticated-controller '<label-item-controller> user params))
           (result (do-get controller)))
      
      (ok (= 404 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 404 Not Found")
      (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
          "Status should be error")
      
      (destroy user))))

(deftest-suite :controller test-label-item-put
  (testing "PUT /api/v1/labels/:ulid updates label"
    (let* ((user (create-user :email "labelupdateuser@example.com"
                              :password-hash "password123"
                              :username "Label Update User"
                              :ulid "01234567890123456789012365"))
           (tag1 (create-tag (ref user :id) "Work-LabelUpdate" :color "#FF5733"))
           (label1 (create-label (ref user :id) "Project C" "Original description" 
                                (list (ref tag1 :ulid))))
           _ (assign-tags-to-label (ref label1 :ulid) (list (ref tag1 :ulid)) (ref user :id))
           (params `((:ulid . ,(ref label1 :ulid))))
           (controller (setup-authenticated-controller '<label-item-controller> user params))
           (body (jonathan:to-json 
                  `(("name" . "Updated Project C")
                    ("description" . "Updated description"))))
           (env (slot-value controller 'clails/controller/base-controller::env)))
      
      (setf (getf env :raw-body) body)
      (let* ((result (do-put controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (label (cdr (assoc "label" data :test #'string=))))
        
        (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 200 OK")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok (string= "Updated Project C" (cdr (assoc "name" label :test #'string=)))
            "Label name should be updated"))
      
      (destroy label1)
      (destroy tag1)
      (destroy user))))

(deftest-suite :controller test-label-item-delete
  (testing "DELETE /api/v1/labels/:ulid deletes label"
    (let* ((user (create-user :email "labeldeleteuser@example.com"
                              :password-hash "password123"
                              :username "Label Delete User"
                              :ulid "01234567890123456789012366"))
           (tag1 (create-tag (ref user :id) "Work-LabelDelete" :color "#FF5733"))
           (label1 (create-label (ref user :id) "Project D" "To be deleted" 
                                (list (ref tag1 :ulid))))
           _ (assign-tags-to-label (ref label1 :ulid) (list (ref tag1 :ulid)) (ref user :id))
           (params `((:ulid . ,(ref label1 :ulid))))
           (controller (setup-authenticated-controller '<label-item-controller> user params))
           (result (do-delete controller)))
      
      (ok (= 204 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 204 No Content")
      
      (destroy tag1)
      (destroy user))))

(deftest-suite :controller test-label-estimate
  (testing "GET /api/v1/labels/estimate-todo-count estimates TODO count"
    (let* ((user (create-user :email "labelestimateuser@example.com"
                              :password-hash "password123"
                              :username "Label Estimate User"
                              :ulid "01234567890123456789012367"))
           (tag1 (create-tag (ref user :id) "Work-LabelEstimate" :color "#FF5733"))
           (controller (setup-authenticated-controller '<label-estimate-controller> user))
           (env (slot-value controller 'clails/controller/base-controller::env)))
      
      (setf (getf env :query-string) (format nil "tag_ulids=~A" (ref tag1 :ulid)))
      (let* ((result (do-get controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (count (cdr (assoc "count" data :test #'string=))))
        
        (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 200 OK")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok (numberp count) "Count should be a number"))
      
      (destroy tag1)
      (destroy user)))
  
  (testing "GET /api/v1/labels/estimate-todo-count fails without tag_ulids"
    (let* ((user (create-user :email "labelestimfailuser@example.com"
                              :password-hash "password123"
                              :username "Label Estim Fail User"
                              :ulid "01234567890123456789012368"))
           (controller (setup-authenticated-controller '<label-estimate-controller> user))
           (result (do-get controller)))
      
      (ok (= 400 (slot-value controller 'clails/controller/base-controller::code))
          "Should return 400 Bad Request")
      (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
          "Status should be error")
      
      (destroy user))))
