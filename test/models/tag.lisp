; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/models/tag
  (:use #:cl
        #:rove
        #:clails/test
        #:clails/model
        #:dogatto/models/tag))
(in-package #:dogatto-test/models/tag)

(deftest-suite :model test-tag-create
  (testing "create-tag creates a tag with valid attributes"
    (let ((tag (create-tag 1 "Work" :color "#3B82F6")))
      (ok tag "Tag should be created")
      (ok (ref tag :ulid) "ULID should be generated")
      (ok (string= (ref tag :name) "Work") "Name should be set")
      (ok (string= (ref tag :color) "#3B82F6") "Color should be set")
      (ok (= (ref tag :owner-id) 1) "Owner ID should be set")
      (destroy tag)))
  
  (testing "create-tag with default color"
    (let ((tag (create-tag 1 "Personal")))
      (ok (string= (ref tag :color) "#3B82F6") "Should use default color")
      (destroy tag)))
  
  (testing "create-tag trims whitespace from name"
    (let ((tag (create-tag 1 "  Trimmed  ")))
      (ok (string= (ref tag :name) "Trimmed") "Name should be trimmed")
      (destroy tag)))
  
  (testing "create-tag fails with empty name"
    (ok (signals (create-tag 1 "")) "Should raise error for empty name"))
  
  (testing "create-tag fails with name too long"
    (ok (signals (create-tag 1 (make-string 51 :initial-element #\a)))
        "Should raise error for name > 50 characters"))
  
  (testing "create-tag fails with invalid color"
    (ok (signals (create-tag 1 "Test" :color "invalid"))
        "Should raise error for invalid color format")))

(deftest-suite :model test-tag-find
  (testing "find-tag-by-id finds existing tag"
    (let* ((tag (create-tag 1 "FindMe"))
           (found (find-tag-by-id (ref tag :id))))
      (ok found "Should find tag by ID")
      (ok (= (ref found :id) (ref tag :id)) "Should return correct tag")
      (destroy tag)))
  
  (testing "find-tag-by-id returns nil for non-existent tag"
    (ok (null (find-tag-by-id 999999)) "Should return nil"))
  
  (testing "find-tag-by-ulid finds existing tag"
    (let* ((tag (create-tag 1 "FindMeByUlid"))
           (found (find-tag-by-ulid (ref tag :ulid))))
      (ok found "Should find tag by ULID")
      (ok (string= (ref found :ulid) (ref tag :ulid)) "Should return correct tag")
      (destroy tag)))
  
  (testing "find-tag-by-ulid returns nil for non-existent tag"
    (ok (null (find-tag-by-ulid "non-existent-ulid")) "Should return nil")))

(deftest-suite :model test-tag-find-by-user
  (testing "find-tags-by-user returns all user tags"
    (let ((tag1 (create-tag 1 "Work-FindByUser"))
          (tag2 (create-tag 1 "Personal-FindByUser"))
          (tag3 (create-tag 2 "Other-FindByUser")))
      (let ((tags (find-tags-by-user 1)))
        (ok (>= (length tags) 2) "Should return at least 2 tags for user 1")
        (ok (member "Work-FindByUser" tags :key #'(lambda (tag) (ref tag :name)) :test #'string=)
            "Should include Work-FindByUser tag")
        (ok (member "Personal-FindByUser" tags :key #'(lambda (tag) (ref tag :name)) :test #'string=)
            "Should include Personal-FindByUser tag"))
      (destroy tag1)
      (destroy tag2)
      (destroy tag3)))
  
  (testing "find-tags-by-user returns empty for user with no tags"
    (ok (null (find-tags-by-user 999)) "Should return empty list")))

(deftest-suite :model test-tag-update
  (testing "update-tag updates name"
    (let ((tag (create-tag 1 "Original")))
      (update-tag tag :name "Updated")
      (ok (string= (ref tag :name) "Updated") "Name should be updated")
      (destroy tag)))
  
  (testing "update-tag updates color"
    (let ((tag (create-tag 1 "ColorTest")))
      (update-tag tag :color "#FF0000")
      (ok (string= (ref tag :color) "#FF0000") "Color should be updated")
      (destroy tag)))
  
  (testing "update-tag validates new name"
    (let ((tag (create-tag 1 "ValidateUpdate")))
      (ok (signals (update-tag tag :name ""))
          "Should raise error for empty name")
      (destroy tag)))
  
  (testing "update-tag validates new color"
    (let ((tag (create-tag 1 "ValidateColor")))
      (ok (signals (update-tag tag :color "invalid"))
          "Should raise error for invalid color")
      (destroy tag))))

(deftest-suite :model test-tag-delete
  (testing "delete-tag deletes the tag"
    (let* ((tag (create-tag 1 "DeleteMe"))
           (id (ref tag :id)))
      (delete-tag tag)
      (ok (null (find-tag-by-id id)) "Tag should be deleted"))))
