; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/models/todo
  (:use #:cl
        #:rove
        #:clails/test
        #:dogatto/models/todo
        #:dogatto/models/user)
  (:import-from #:clails/model
                #:ref
                #:destroy))
(in-package #:dogatto-test/models/todo)

(deftest-suite :model test-create-todo
  (testing "Create a new TODO successfully"
    (let ((user (create-user :username "Test User" 
                            :email "todo-test@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012340")))
      (unwind-protect
           (let ((todo (create-todo (ref user :id)
                                   "Buy milk"
                                   :content "Get 2 liters of milk"
                                   :due-date (get-universal-time))))
             (ok (not (null todo)) "TODO should be created")
             (ok (string= (ref todo :title) "Buy milk")
                 "Title should match")
             (ok (string= (ref todo :content) "Get 2 liters of milk")
                 "Content should match")
             (ok (string= (ref todo :status) "active")
                 "Status should be active by default")
             (ok (ref todo :ulid)
                 "ULID should be generated")
             (delete-todo todo))
        (destroy user)))))

(deftest-suite :model test-create-todo-validation
  (testing "Fail to create TODO without title"
    (let ((user (create-user :username "Test User" 
                            :email "todo-val@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012341")))
      (unwind-protect
           (ok (signals (create-todo (ref user :id) nil)
                        'simple-error)
               "Should raise error when title is nil")
        (destroy user))))
  
  (testing "Fail to create TODO with empty title"
    (let ((user (create-user :username "Test User" 
                            :email "todo-empty@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012342")))
      (unwind-protect
           (ok (signals (create-todo (ref user :id) "")
                        'simple-error)
               "Should raise error when title is empty")
        (destroy user))))
  
  (testing "Fail to create TODO with too long title"
    (let ((user (create-user :username "Test User" 
                            :email "todo-long@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012343")))
      (unwind-protect
           (ok (signals (create-todo (ref user :id)
                                    (make-string 256 :initial-element #\a))
                        'simple-error)
               "Should raise error when title exceeds 255 characters")
        (destroy user)))))

(deftest-suite :model test-find-todo-by-id
  (testing "Find TODO by ID"
    (let ((user (create-user :username "Test User" 
                            :email "todo-find@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012344")))
      (unwind-protect
           (let* ((todo (create-todo (ref user :id) "Find me"))
                  (found (find-todo-by-id (ref todo :id))))
             (ok (not (null found)) "TODO should be found")
             (ok (= (ref found :id)
                    (ref todo :id))
                 "Found TODO should have same ID")
             (delete-todo todo))
        (destroy user))))
  
  (testing "Return nil when TODO not found"
    (let ((found (find-todo-by-id 999999)))
      (ok (null found) "Should return nil for non-existent ID"))))

(deftest-suite :model test-find-todos-by-user
  (testing "Find all TODOs for a user"
    (let ((user (create-user :username "Test User" 
                            :email "todo-list@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012345")))
      (unwind-protect
           (let ((todo1 (create-todo (ref user :id) "TODO 1"))
                 (todo2 (create-todo (ref user :id) "TODO 2")))
             (let ((todos (find-todos-by-user (ref user :id))))
               (ok (>= (length todos) 2) "Should find at least 2 TODOs")
               (delete-todo todo1)
               (delete-todo todo2)))
        (destroy user))))
  
  (testing "Return empty list when user has no TODOs"
    (let ((user (create-user :username "No TODOs" 
                            :email "notodos@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012346")))
      (unwind-protect
           (let ((todos (find-todos-by-user (ref user :id))))
             (ok (= (length todos) 0) "Should return empty list"))
        (destroy user)))))

(deftest-suite :model test-update-todo
  (testing "Update TODO successfully"
    (let ((user (create-user :username "Test User" 
                            :email "todo-update@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012347")))
      (unwind-protect
           (let ((todo (create-todo (ref user :id) "Original")))
             (update-todo todo :title "Updated" :content "New content")
             (ok (string= (ref todo :title) "Updated")
                 "Title should be updated")
             (ok (string= (ref todo :content) "New content")
                 "Content should be updated")
             (delete-todo todo))
        (destroy user)))))

(deftest-suite :model test-delete-todo
  (testing "Delete TODO successfully"
    (let ((user (create-user :username "Test User" 
                            :email "todo-delete@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012348")))
      (unwind-protect
           (let* ((todo (create-todo (ref user :id) "To be deleted"))
                  (todo-id (ref todo :id)))
             (delete-todo todo)
             (ok (null (find-todo-by-id todo-id))
                 "TODO should not be found after deletion"))
        (destroy user)))))

(deftest-suite :model test-toggle-todo-status
  (testing "Toggle TODO from active to completed"
    (let ((user (create-user :username "Test User" 
                            :email "todo-toggle@example.com" 
                            :password-hash "hashedpass"
                            :ulid "01234567890123456789012349")))
      (unwind-protect
           (let ((todo (create-todo (ref user :id) "Toggle me")))
             (toggle-todo-status todo)
             (ok (string= (ref todo :status) "completed")
                 "Status should be completed")
             (ok (not (null (ref todo :completed-at)))
                 "Completed-at should be set")
             
             (toggle-todo-status todo)
             (ok (string= (ref todo :status) "active")
                 "Status should be active again")
             (ok (null (ref todo :completed-at))
                 "Completed-at should be cleared")
             (delete-todo todo))
        (destroy user)))))
