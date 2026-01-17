; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/utils/session
  (:use #:cl
        #:rove
        #:clails/test
        #:dogatto/utils/session))
(in-package #:dogatto-test/utils/session)

(deftest-suite :utils test-generate-session-id
  (testing "generate-session-id returns a UUID string"
    (let ((session-id (generate-session-id)))
      (ok (stringp session-id))
      (ok (> (length session-id) 0))
      ;; UUID format: 8-4-4-4-12 characters with hyphens
      (ok (= (length session-id) 36))
      (ok (char= (char session-id 8) #\-))
      (ok (char= (char session-id 13) #\-))
      (ok (char= (char session-id 18) #\-))
      (ok (char= (char session-id 23) #\-))))
  
  (testing "generate-session-id generates unique IDs"
    (let ((id1 (generate-session-id))
          (id2 (generate-session-id)))
      (ok (not (string= id1 id2))))))

(deftest-suite :utils test-create-session
  (testing "create-session returns session ID"
    (let ((session-id (create-session 123)))
      (ok (stringp session-id))
      (ok (= (length session-id) 36))
      ;; Cleanup
      (delete-session session-id)))
  
  (testing "create-session with custom TTL"
    (let ((session-id (create-session 456 :ttl 3600)))
      (ok (stringp session-id))
      ;; Cleanup
      (delete-session session-id))))

(deftest-suite :utils test-get-session
  (testing "get-session retrieves stored session"
    (let ((session-id (create-session 789)))
      (let ((session-data (get-session session-id)))
        (ok (not (null session-data)))
        (ok (= (getf session-data :user-id) 789))
        (ok (integerp (getf session-data :created-at)))
        (ok (integerp (getf session-data :expires-at)))
        (ok (> (getf session-data :expires-at) (getf session-data :created-at))))
      ;; Cleanup
      (delete-session session-id)))
  
  (testing "get-session returns nil for non-existent session"
    (let ((session-data (get-session "non-existent-uuid")))
      (ok (null session-data)))))

(deftest-suite :utils test-delete-session
  (testing "delete-session removes session"
    (let ((session-id (create-session 111)))
      ;; Verify session exists
      (ok (not (null (get-session session-id))))
      ;; Delete session
      (ok (delete-session session-id))
      ;; Verify session is gone
      (ok (null (get-session session-id)))))
  
  (testing "delete-session returns nil for non-existent session"
    (ok (not (delete-session "non-existent-uuid")))))

(deftest-suite (:utils :validation) test-session-valid-p
  (testing "session-valid-p returns T for valid session"
    (let ((session-id (create-session 222)))
      (ok (session-valid-p session-id))
      ;; Cleanup
      (delete-session session-id)))
  
  (testing "session-valid-p returns NIL for non-existent session"
    (ok (not (session-valid-p "non-existent-uuid"))))
  
  (testing "session-valid-p returns NIL for expired session"
    ;; Create session with very short TTL
    (let ((session-id (create-session 333 :ttl 1)))
      ;; Session should be valid immediately
      (ok (session-valid-p session-id))
      ;; Wait for expiration
      (sleep 2)
      ;; Session should be expired (Redis will auto-delete)
      (ok (not (session-valid-p session-id))))))

(deftest-suite (:utils :integration) test-session-workflow
  (testing "full session workflow"
    (let* ((user-id 999)
           (session-id (create-session user-id)))
      
      ;; Create: Session ID should be generated
      (ok (stringp session-id))
      (ok (= (length session-id) 36))
      
      ;; Get: Session should exist with correct user-id
      (let ((session-data (get-session session-id)))
        (ok (not (null session-data)))
        (ok (= (getf session-data :user-id) user-id)))
      
      ;; Valid: Session should be valid
      (ok (session-valid-p session-id))
      
      ;; Delete: Session should be removed
      (ok (delete-session session-id))
      
      ;; Get after delete: Should return nil
      (ok (null (get-session session-id)))
      
      ;; Valid after delete: Should return nil
      (ok (not (session-valid-p session-id))))))
