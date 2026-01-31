; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/todo-tags-controller
  (:use #:cl
        #:rove
        #:clails/test)
  (:import-from #:dogatto/controllers/todo-tags-controller
                #:<todo-tags-controller>
                #:do-get
                #:do-put)
  (:import-from #:dogatto/models/user
                #:create-user
                #:<user>)
  (:import-from #:dogatto/models/todo
                #:create-todo
                #:<todo>)
  (:import-from #:dogatto/models/tag
                #:create-tag
                #:<tag>)
  (:import-from #:dogatto/models/todo-tag
                #:assign-tags-to-todo
                #:find-tags-for-todo)
  (:import-from #:dogatto/utils/session
                #:create-session
                #:delete-session)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:clails/controller/base-controller
                #:response))
(in-package #:dogatto-test/controllers/todo-tags-controller)

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

(deftest test-todo-tags-get
  (testing "GET /api/v1/todos/:ulid/tags returns tags for a TODO"
    (with-transaction
      (let* ((user (create-user "taguser@example.com" "password123" "Tag User"))
             (todo (create-todo (ref user :id) "Tagged TODO" "Content"))
             (tag1 (create-tag (ref user :id) "Work" :color "#FF5733"))
             (tag2 (create-tag (ref user :id) "Urgent" :color "#33FF57"))
             (todo-ulid (ref todo :ulid))
             (tag1-ulid (ref tag1 :ulid))
             (tag2-ulid (ref tag2 :ulid)))
        
        ;; Assign tags to TODO
        (assign-tags-to-todo todo-ulid (list tag1-ulid tag2-ulid))
        
        (let* ((controller (setup-authenticated-controller 
                           '<todo-tags-controller>
                           user
                           (list (cons "ulid" todo-ulid))))
               (result (do-get controller))
               (data (cdr (assoc "data" result :test #'string=)))
               (tags (cdr (assoc "tags" data :test #'string=))))
          
          (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
              "Should return 200 OK")
          (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
              "Status should be success")
          (ok (= 2 (length tags))
              "Should return 2 tags")))))
  
  (testing "GET /api/v1/todos/:ulid/tags fails without authentication"
    (with-transaction
      (let* ((user (create-user "taguser2@example.com" "password123" "Tag User 2"))
             (todo (create-todo (ref user :id) "Tagged TODO" "Content"))
             (todo-ulid (ref todo :ulid))
             (controller (make-instance '<todo-tags-controller>))
             (env (list :cookies '())))
        
        (setf (slot-value controller 'clails/controller/base-controller::env) env)
        (setf (gethash "ulid" (slot-value controller 'clails/controller/base-controller::params))
              todo-ulid)
        
        (let ((result (do-get controller)))
          (ok (= 401 (slot-value controller 'clails/controller/base-controller::code))
              "Should return 401 Unauthorized")
          (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
              "Status should be error")))))
  
  (testing "GET /api/v1/todos/:ulid/tags fails for non-existent TODO"
    (with-transaction
      (let* ((user (create-user "taguser3@example.com" "password123" "Tag User 3"))
             (controller (setup-authenticated-controller 
                         '<todo-tags-controller>
                         user
                         (list (cons "ulid" "01NONEXISTENT0000000000000"))))
             (result (do-get controller)))
        
        (ok (= 404 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 404 Not Found")
        (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
            "Status should be error")))))

(deftest test-todo-tags-put
  (testing "PUT /api/v1/todos/:ulid/tags assigns tags to TODO"
    (with-transaction
      (let* ((user (create-user "taguser4@example.com" "password123" "Tag User 4"))
             (todo (create-todo (ref user :id) "TODO to tag" "Content"))
             (tag1 (create-tag (ref user :id) "Important" :color "#FF5733"))
             (tag2 (create-tag (ref user :id) "Review" :color "#33FF57"))
             (todo-ulid (ref todo :ulid))
             (tag1-ulid (ref tag1 :ulid))
             (tag2-ulid (ref tag2 :ulid))
             (controller (setup-authenticated-controller 
                         '<todo-tags-controller>
                         user
                         (list (cons "ulid" todo-ulid)
                               (cons "tagUlids" (list tag1-ulid tag2-ulid)))))
             (result (do-put controller))
             (data (cdr (assoc "data" result :test #'string=)))
             (tags (cdr (assoc "tags" data :test #'string=))))
        
        (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
            "Should return 200 OK")
        (ok (string= "success" (cdr (assoc "status" result :test #'string=)))
            "Status should be success")
        (ok (= 2 (length tags))
            "Should have 2 tags assigned"))))
  
  (testing "PUT /api/v1/todos/:ulid/tags replaces existing tags"
    (with-transaction
      (let* ((user (create-user "taguser5@example.com" "password123" "Tag User 5"))
             (todo (create-todo (ref user :id) "TODO with tags" "Content"))
             (tag1 (create-tag (ref user :id) "Old" :color "#FF5733"))
             (tag2 (create-tag (ref user :id) "New" :color "#33FF57"))
             (todo-ulid (ref todo :ulid))
             (tag1-ulid (ref tag1 :ulid))
             (tag2-ulid (ref tag2 :ulid)))
        
        ;; First assign tag1
        (assign-tags-to-todo todo-ulid (list tag1-ulid))
        
        ;; Then replace with tag2
        (let* ((controller (setup-authenticated-controller 
                           '<todo-tags-controller>
                           user
                           (list (cons "ulid" todo-ulid)
                                 (cons "tagUlids" (list tag2-ulid)))))
               (result (do-put controller))
               (data (cdr (assoc "data" result :test #'string=)))
               (tags (cdr (assoc "tags" data :test #'string=))))
          
          (ok (= 200 (slot-value controller 'clails/controller/base-controller::code))
              "Should return 200 OK")
          (ok (= 1 (length tags))
              "Should have only 1 tag")
          (ok (string= tag2-ulid (cdr (assoc "ulid" (first tags) :test #'string=)))
              "Should be the new tag")))))
  
  (testing "PUT /api/v1/todos/:ulid/tags fails without authentication"
    (with-transaction
      (let* ((user (create-user "taguser6@example.com" "password123" "Tag User 6"))
             (todo (create-todo (ref user :id) "TODO" "Content"))
             (todo-ulid (ref todo :ulid))
             (controller (make-instance '<todo-tags-controller>))
             (env (list :cookies '())))
        
        (setf (slot-value controller 'clails/controller/base-controller::env) env)
        (setf (gethash "ulid" (slot-value controller 'clails/controller/base-controller::params))
              todo-ulid)
        
        (let ((result (do-put controller)))
          (ok (= 401 (slot-value controller 'clails/controller/base-controller::code))
              "Should return 401 Unauthorized")
          (ok (string= "error" (cdr (assoc "status" result :test #'string=)))
              "Status should be error"))))))
