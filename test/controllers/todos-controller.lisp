; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/controllers/todos-controller
  (:use #:cl
        #:rove
        #:clails/test)
  (:import-from #:dogatto/controllers/todos-controller
                #:<todos-list-controller>
                #:<todo-item-controller>
                #:<todo-complete-controller>
                #:do-get
                #:do-post
                #:do-put
                #:do-delete)
  (:import-from #:dogatto/models/user
                #:create-user
                #:<user>)
  (:import-from #:dogatto/models/todo
                #:create-todo
                #:find-todo-by-id
                #:<todo>)
  (:import-from #:dogatto/utils/session
                #:create-session
                #:delete-session)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:clails/controller/base-controller
                #:response))
(in-package #:dogatto-test/controllers/todos-controller)

(defun setup-authenticated-controller (controller-class user &optional params)
  "Setup controller with authentication session.

   @param controller-class [symbol] Controller class to instantiate
   @param user [<user>] User instance for authentication
   @param params [alist] Optional parameters to set (optional)
   @return [<base-controller>] Initialized controller instance
   "
  (let* ((session-id (create-session (ref user :id)))
         (controller (make-instance controller-class))
         (env (list :headers (make-hash-table :test 'equal)
                   :cookies (list (cons "session_id" session-id))
                   :current-user user)))
    (setf (slot-value controller 'clails/controller/base-controller::env) env)
    (when params
      (loop for (key . value) in params
            do (setf (gethash key (slot-value controller 'clails/controller/base-controller::params))
                    value)))
    controller))

(deftest-suite :controller test-todos-list-get
  (testing "GET /api/v1/todos returns todos for authenticated user"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-test@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012350"))
           (todo1 (create-todo (ref user :id) "Test Todo 1"))
           (todo2 (create-todo (ref user :id) "Test Todo 2"))
           (controller (setup-authenticated-controller '<todos-list-controller> user)))
      (unwind-protect
           (progn
             (do-get controller)
             (let ((resp (response controller)))
               (ok (= (slot-value controller 'clails/controller/base-controller::code) 200)
                   "Should return 200 OK")
               (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
                   "Status should be success")
               (ok (assoc "data" resp :test #'string=)
                   "Should have data")))
        (when todo1 (destroy todo1))
        (when todo2 (destroy todo2))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "GET /api/v1/todos fails without authentication"
    (let ((controller (make-instance '<todos-list-controller>)))
      (setf (slot-value controller 'clails/controller/base-controller::env)
            (list :headers (make-hash-table :test 'equal)
                  :cookies nil))
      (do-get controller)
      (ok (= (slot-value controller 'clails/controller/base-controller::code) 401)
          "Should return 401 Unauthorized")
      (let ((resp (response controller)))
        (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
            "Status should be error")))))

(deftest-suite :controller test-todos-list-post
  (testing "POST /api/v1/todos creates a new todo"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-create@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012351"))
           (controller (setup-authenticated-controller 
                       '<todos-list-controller> 
                       user
                       '(("title" . "New Todo")
                         ("content" . "Todo content")))))
      (unwind-protect
           (progn
             (do-post controller)
             (let ((resp (response controller)))
               (ok (= (slot-value controller 'clails/controller/base-controller::code) 201)
                   "Should return 201 Created")
               (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
                   "Status should be success")
               (let* ((data (cdr (assoc "data" resp :test #'string=)))
                      (todo (cdr (assoc "todo" data :test #'string=))))
                 (ok todo "Should have todo in response")
                 (ok (string= (cdr (assoc "title" todo :test #'string=)) "New Todo")
                     "Title should match"))))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "POST /api/v1/todos fails without title"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-notitle@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012352"))
           (controller (setup-authenticated-controller 
                       '<todos-list-controller> 
                       user
                       '(("content" . "No title")))))
      (unwind-protect
           (progn
             (do-post controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 400)
                 "Should return 400 Bad Request")
             (let ((resp (response controller)))
               (ok (string= (cdr (assoc "status" resp :test #'string=)) "error")
                   "Status should be error")))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "POST /api/v1/todos fails without authentication"
    (let ((controller (make-instance '<todos-list-controller>)))
      (setf (slot-value controller 'clails/controller/base-controller::env)
            (list :headers (make-hash-table :test 'equal)
                  :cookies nil))
      (setf (gethash "title" (slot-value controller 'clails/controller/base-controller::params))
            "Test")
      (do-post controller)
      (ok (= (slot-value controller 'clails/controller/base-controller::code) 401)
          "Should return 401 Unauthorized"))))

(deftest-suite :controller test-todo-item-get
  (testing "GET /api/v1/todos/:id returns specific todo"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-get@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012353"))
           (todo (create-todo (ref user :id) "Get This Todo"))
           (controller (setup-authenticated-controller 
                       '<todo-item-controller>
                       user
                       `(("id" . ,(format nil "~A" (ref todo :id)))))))
      (unwind-protect
           (progn
             (do-get controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 200)
                 "Should return 200 OK")
             (let ((resp (response controller)))
               (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
                   "Status should be success")))
        (when todo (destroy todo))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "GET /api/v1/todos/:id fails for non-existent todo"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-notfound@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012354"))
           (controller (setup-authenticated-controller 
                       '<todo-item-controller>
                       user
                       '(("id" . "999999")))))
      (unwind-protect
           (progn
             (do-get controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 404)
                 "Should return 404 Not Found"))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "GET /api/v1/todos/:id fails for other user's todo"
    (let* ((user1 (create-user :username "User 1"
                              :email "user1@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012355"))
           (user2 (create-user :username "User 2"
                              :email "user2@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012356"))
           (todo (create-todo (ref user1 :id) "User 1's Todo"))
           (controller (setup-authenticated-controller 
                       '<todo-item-controller>
                       user2
                       `(("id" . ,(format nil "~A" (ref todo :id)))))))
      (unwind-protect
           (progn
             (do-get controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 403)
                 "Should return 403 Forbidden"))
        (when todo (destroy todo))
        (delete-session (ref user1 :id))
        (delete-session (ref user2 :id))
        (destroy user1)
        (destroy user2)))))

(deftest-suite :controller test-todo-item-put
  (testing "PUT /api/v1/todos/:id updates todo"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-update@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012357"))
           (todo (create-todo (ref user :id) "Original Title"))
           (controller (setup-authenticated-controller 
                       '<todo-item-controller>
                       user
                       `(("id" . ,(format nil "~A" (ref todo :id)))
                         ("title" . "Updated Title")))))
      (unwind-protect
           (progn
             (do-put controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 200)
                 "Should return 200 OK")
             (let ((resp (response controller)))
               (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
                   "Status should be success")))
        (when todo (destroy todo))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "PUT /api/v1/todos/:id fails for other user's todo"
    (let* ((user1 (create-user :username "User 1"
                              :email "user1-update@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012358"))
           (user2 (create-user :username "User 2"
                              :email "user2-update@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012359"))
           (todo (create-todo (ref user1 :id) "User 1's Todo"))
           (controller (setup-authenticated-controller 
                       '<todo-item-controller>
                       user2
                       `(("id" . ,(format nil "~A" (ref todo :id)))
                         ("title" . "Hacked")))))
      (unwind-protect
           (progn
             (do-put controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 403)
                 "Should return 403 Forbidden"))
        (when todo (destroy todo))
        (delete-session (ref user1 :id))
        (delete-session (ref user2 :id))
        (destroy user1)
        (destroy user2)))))

(deftest-suite :controller test-todo-item-delete
  (testing "DELETE /api/v1/todos/:id deletes todo"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-delete@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012360"))
           (todo (create-todo (ref user :id) "Delete Me"))
           (controller (setup-authenticated-controller 
                       '<todo-item-controller>
                       user
                       `(("id" . ,(format nil "~A" (ref todo :id)))))))
      (unwind-protect
           (progn
             (do-delete controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 200)
                 "Should return 200 OK")
             (let ((resp (response controller)))
               (ok (string= (cdr (assoc "status" resp :test #'string=)) "success")
                   "Status should be success")))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "DELETE /api/v1/todos/:id fails for other user's todo"
    (let* ((user1 (create-user :username "User 1"
                              :email "user1-delete@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012361"))
           (user2 (create-user :username "User 2"
                              :email "user2-delete@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012362"))
           (todo (create-todo (ref user1 :id) "User 1's Todo"))
           (controller (setup-authenticated-controller 
                       '<todo-item-controller>
                       user2
                       `(("id" . ,(format nil "~A" (ref todo :id)))))))
      (unwind-protect
           (progn
             (do-delete controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 403)
                 "Should return 403 Forbidden"))
        (when todo (destroy todo))
        (delete-session (ref user1 :id))
        (delete-session (ref user2 :id))
        (destroy user1)
        (destroy user2)))))

(deftest-suite :controller test-todo-complete-put
  (testing "PUT /api/v1/todos/:id/complete toggles todo status"
    (let* ((user (create-user :username "Todo User"
                             :email "todo-complete@example.com"
                             :password-hash "hashedpass"
                             :ulid "01234567890123456789012363"))
           (todo (create-todo (ref user :id) "Complete Me"))
           (controller (setup-authenticated-controller 
                       '<todo-complete-controller>
                       user
                       `(("id" . ,(format nil "~A" (ref todo :id)))))))
      (unwind-protect
           (progn
             (ok (string= (ref todo :status) "active")
                 "Todo should start as active")
             (do-put controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 200)
                 "Should return 200 OK")
             (let* ((resp (response controller))
                    (data (cdr (assoc "data" resp :test #'string=)))
                    (todo-data (cdr (assoc "todo" data :test #'string=))))
               (ok (string= (cdr (assoc "status" todo-data :test #'string=)) "completed")
                   "Todo status should be completed")))
        (when todo (destroy todo))
        (delete-session (ref user :id))
        (destroy user))))
  
  (testing "PUT /api/v1/todos/:id/complete fails for other user's todo"
    (let* ((user1 (create-user :username "User 1"
                              :email "user1-complete@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012364"))
           (user2 (create-user :username "User 2"
                              :email "user2-complete@example.com"
                              :password-hash "hashedpass"
                              :ulid "01234567890123456789012365"))
           (todo (create-todo (ref user1 :id) "User 1's Todo"))
           (controller (setup-authenticated-controller 
                       '<todo-complete-controller>
                       user2
                       `(("id" . ,(format nil "~A" (ref todo :id)))))))
      (unwind-protect
           (progn
             (do-put controller)
             (ok (= (slot-value controller 'clails/controller/base-controller::code) 403)
                 "Should return 403 Forbidden"))
        (when todo (destroy todo))
        (delete-session (ref user1 :id))
        (delete-session (ref user2 :id))
        (destroy user1)
        (destroy user2)))))
