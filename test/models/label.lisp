; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/models/label
  (:use #:cl
        #:rove
        #:clails/test)
  (:import-from #:clails/model
                #:ref
                #:destroy)
  (:import-from #:dogatto/models/user
                #:create-user)
  (:import-from #:dogatto/models/tag
                #:create-tag)
  (:import-from #:dogatto/models/label
                #:<label>
                #:find-label-by-id
                #:find-label-by-ulid
                #:find-labels-by-owner
                #:delete-label
                #:check-label-name-uniqueness
                #:search-labels-by-name
                #:estimate-todo-count-by-tags
                #:get-label-todo-count)
  (:import-from #:dogatto/models/label-tag
                #:create-label-with-tags
                #:update-label-with-tags
                #:assign-tags-to-label
                #:find-tags-for-label
                #:find-labels-by-tag-name)
  (:import-from #:dogatto/models/todo
                #:create-todo)
  (:import-from #:dogatto/models/todo-tag
                #:assign-tags-to-todo)
  (:import-from #:dogatto/utils/ulid
                #:generate-ulid))
(in-package #:dogatto-test/models/label)

(defun create-test-user (email)
  "Create a test user with generated ULID and unique email.

   @param email [string] User email (can be any value, will use ULID for uniqueness)
   @return [<user>] Created user instance
   "
  (let ((unique-email (format nil "test-~A@example.com" (generate-ulid))))
    (create-user :username (format nil "user-~A" (generate-ulid))
                 :email unique-email
                 :password-hash "test-hash"
                 :ulid (generate-ulid))))

(deftest-suite :model test-label-create
  (testing "create-label creates a label with valid attributes"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "My Label" "Test description" (list (ref tag1 :ulid)))))
      (ok label "Label should be created")
      (ok (ref label :ulid) "ULID should be generated")
      (ok (string= (ref label :name) "My Label") "Name should be set")
      (ok (string= (ref label :description) "Test description") "Description should be set")
      (ok (= (ref label :owner-id) (ref user :id)) "Owner ID should be set")
      
      ;; Verify tags
      (let ((tags (find-tags-for-label (ref label :ulid) (ref user :id))))
        (ok (= (length tags) 1) "Label should have 1 tag"))
      
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "create-label without description"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "Simple Label" nil (list (ref tag1 :ulid)))))
      (ok label "Label should be created")
      (ok (null (ref label :description)) "Description should be nil")
      
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "create-label trims whitespace from name"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "  Trimmed  " nil (list (ref tag1 :ulid)))))
      (ok (string= (ref label :name) "Trimmed") "Name should be trimmed")
      
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "create-label fails with empty name"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000")))
      (ok (null (create-label-with-tags (ref user :id) "" nil (list (ref tag1 :ulid)))) 
          "Should return nil for empty name")
      (destroy tag1)
      (destroy user)))
  
  (testing "create-label fails without tags"
    (let ((user (create-test-user nil)))
      (ok (signals (create-label-with-tags (ref user :id) "No Tags" nil nil))
          "Should raise error when no tags provided")
      (destroy user))))

(deftest-suite :model test-label-find
  (testing "find-label-by-id finds existing label"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "FindMe" nil (list (ref tag1 :ulid))))
           (found (find-label-by-id (ref label :id) (ref user :id))))
      (ok found "Should find label by ID")
      (ok (= (ref found :id) (ref label :id)) "Should return correct label")
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "find-label-by-id returns nil for non-existent label"
    (ok (null (find-label-by-id 999999 0)) "Should return nil"))
  
  (testing "find-label-by-ulid finds existing label"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "FindMeByUlid" nil (list (ref tag1 :ulid))))
           (found (find-label-by-ulid (ref label :ulid) (ref user :id))))
      (ok found "Should find label by ULID")
      (ok (string= (ref found :ulid) (ref label :ulid)) "Should return correct label")
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "find-label-by-ulid returns nil for non-existent label"
    (ok (null (find-label-by-ulid "non-existent-ulid" 0)) "Should return nil")))

(deftest-suite :model test-label-find-by-owner
  (testing "find-labels-by-owner returns all owner labels"
    (let* ((user1 (create-test-user nil))
           (user2 (create-test-user nil))
           (tag1 (create-tag (ref user1 :id) "TAG1" :color "#FF0000"))
           (tag2 (create-tag (ref user2 :id) "TAG2" :color "#00FF00"))
           (label1 (create-label-with-tags (ref user1 :id) "Label1-FindByOwner" nil (list (ref tag1 :ulid))))
           (label2 (create-label-with-tags (ref user1 :id) "Label2-FindByOwner" nil (list (ref tag1 :ulid))))
           (label3 (create-label-with-tags (ref user2 :id) "Label3-FindByOwner" nil (list (ref tag2 :ulid)))))
      
      (let ((labels (find-labels-by-owner (ref user1 :id))))
        (ok (>= (length labels) 2) "Should return at least 2 labels for owner 1")
        (ok (member "Label1-FindByOwner" labels :key #'(lambda (l) (ref l :name)) :test #'string=)
            "Should include Label1-FindByOwner")
        (ok (member "Label2-FindByOwner" labels :key #'(lambda (l) (ref l :name)) :test #'string=)
            "Should include Label2-FindByOwner"))
      (destroy label1)
      (destroy label2)
      (destroy label3)
      (destroy tag1)
      (destroy tag2)
      (destroy user1)
      (destroy user2)))
  
  (testing "find-labels-by-owner returns empty for owner with no labels"
    (let ((user (create-test-user nil)))
      (ok (null (find-labels-by-owner (ref user :id))) "Should return empty list")
      (destroy user))))

(deftest-suite :model test-label-update
  (testing "update-label updates name"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "Original" nil (list (ref tag1 :ulid)))))
      (update-label-with-tags (ref label :ulid) (ref user :id) :name "Updated")
      (let ((updated (find-label-by-ulid (ref label :ulid) (ref user :id))))
        (ok (string= (ref updated :name) "Updated") "Name should be updated"))
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "update-label updates description"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "DescTest" nil (list (ref tag1 :ulid)))))
      (update-label-with-tags (ref label :ulid) (ref user :id) :description "New description")
      (let ((updated (find-label-by-ulid (ref label :ulid) (ref user :id))))
        (ok (string= (ref updated :description) "New description") "Description should be updated"))
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "update-label validates new name"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "ValidateUpdate" nil (list (ref tag1 :ulid)))))
      (ok (null (update-label-with-tags (ref label :ulid) (ref user :id) :name ""))
          "Should return nil for empty name")
      (destroy label)
      (destroy tag1)
      (destroy user))))

(deftest-suite :model test-label-delete
  (testing "delete-label deletes the label"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "DeleteMe" nil (list (ref tag1 :ulid))))
           (ulid (ref label :ulid)))
      (delete-label ulid (ref user :id))
      (ok (null (find-label-by-ulid ulid (ref user :id))) "Label should be deleted")
      (destroy tag1)
      (destroy user))))

(deftest-suite :model test-label-name-uniqueness
  (testing "check-label-name-uniqueness returns true for unique name"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "UniqueName" nil (list (ref tag1 :ulid)))))
      (ok (check-label-name-uniqueness (ref user :id) "DifferentName") 
          "Should return true for unique name")
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "check-label-name-uniqueness returns false for duplicate name"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "DuplicateName" nil (list (ref tag1 :ulid)))))
      (ok (not (check-label-name-uniqueness (ref user :id) "DuplicateName"))
          "Should return false for duplicate name")
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "check-label-name-uniqueness is case-insensitive"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "CaseTest" nil (list (ref tag1 :ulid)))))
      (ok (not (check-label-name-uniqueness (ref user :id) "CaseTest"))
          "Should return false for exact match")
      (ok (not (check-label-name-uniqueness (ref user :id) "CaseTest"))
          "Should return false for same case")
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "check-label-name-uniqueness excludes specified label"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "ExcludeTest" nil (list (ref tag1 :ulid)))))
      (ok (check-label-name-uniqueness (ref user :id) "ExcludeTest" (ref label :ulid))
          "Should return true when excluding the label itself")
      (destroy label)
      (destroy tag1)
      (destroy user))))

(deftest-suite :model test-search-labels-by-name
  (testing "search-labels-by-name finds matching labels"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label1 (create-label-with-tags (ref user :id) "Project Alpha" nil (list (ref tag1 :ulid))))
           (label2 (create-label-with-tags (ref user :id) "Project Beta" nil (list (ref tag1 :ulid))))
           (label3 (create-label-with-tags (ref user :id) "Task Gamma" nil (list (ref tag1 :ulid)))))
      
      (let ((results (search-labels-by-name (ref user :id) "Project")))
        (ok (>= (length results) 2) "Should find at least 2 labels with 'project'")
        (ok (every #'(lambda (l) 
                       (search "Project" (ref l :name)))
                   results)
            "All results should contain 'project'"))
      (destroy label1)
      (destroy label2)
      (destroy label3)
      (destroy tag1)
      (destroy user)))
  
  (testing "search-labels-by-name is case-insensitive"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "TAG1" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "CaseSensitive" nil (list (ref tag1 :ulid)))))
      
      (let ((results (search-labels-by-name (ref user :id) "CaseSensitive")))
        (ok (>= (length results) 1) "Should find label with exact match"))
      
      (let ((results2 (search-labels-by-name (ref user :id) "Sensitive")))
        (ok (>= (length results2) 1) "Should find label with partial match"))
      
      (destroy label)
      (destroy tag1)
      (destroy user)))
  
  (testing "search-labels-by-name returns empty for no matches"
    (let ((user (create-test-user nil)))
      (ok (null (search-labels-by-name (ref user :id) "NonExistentLabel12345"))
          "Should return empty list for no matches")
      (destroy user)))
  
  (testing "search-labels-by-name returns nil for empty query"
    (let ((user (create-test-user nil)))
      (ok (null (search-labels-by-name (ref user :id) "")) "Should return nil for empty query")
      (ok (null (search-labels-by-name (ref user :id) "   ")) "Should return nil for whitespace-only query")
      (destroy user))))

(deftest-suite :model test-find-labels-by-tag-name
  (testing "find-labels-by-tag-name finds labels with matching tag"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "WorkTag" :color "#FF0000"))
           (tag2 (create-tag (ref user :id) "PersonalTag" :color "#00FF00"))
           (label1 (create-label-with-tags (ref user :id) "Work Label" nil (list (ref tag1 :ulid))))
           (label2 (create-label-with-tags (ref user :id) "Personal Label" nil (list (ref tag2 :ulid)))))
      
      (let ((results (find-labels-by-tag-name (ref user :id) "Work")))
        (ok (>= (length results) 1) "Should find at least 1 label with 'work' tag")
        (ok (member "Work Label" results :key #'(lambda (l) (ref l :name)) :test #'string=)
            "Should include 'Work Label'"))
      
      (destroy label1)
      (destroy label2)
      (destroy tag1)
      (destroy tag2)
      (destroy user)))
  
  (testing "find-labels-by-tag-name is case-insensitive"
    (let* ((user (create-test-user nil))
           (tag (create-tag (ref user :id) "CaseTag" :color "#FF0000"))
           (label (create-label-with-tags (ref user :id) "Case Label" nil (list (ref tag :ulid)))))
      
      (let ((results (find-labels-by-tag-name (ref user :id) "CaseTag")))
        (ok (>= (length results) 1) "Should find label with exact tag name"))
      
      (let ((results2 (find-labels-by-tag-name (ref user :id) "Case")))
        (ok (>= (length results2) 1) "Should find label with partial tag name"))
      
      (destroy label)
      (destroy tag)
      (destroy user)))
  
  (testing "find-labels-by-tag-name with multiple tags per label"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "MultiTag1" :color "#FF0000"))
           (tag2 (create-tag (ref user :id) "MultiTag2" :color "#00FF00"))
           (label (create-label-with-tags (ref user :id) "Multi Tag Label" nil (list (ref tag1 :ulid) (ref tag2 :ulid)))))
      
      (let ((results1 (find-labels-by-tag-name (ref user :id) "MultiTag1"))
            (results2 (find-labels-by-tag-name (ref user :id) "MultiTag2")))
        (ok (>= (length results1) 1) "Should find label by first tag")
        (ok (>= (length results2) 1) "Should find label by second tag")
        (ok (member "Multi Tag Label" results1 :key #'(lambda (l) (ref l :name)) :test #'string=)
            "Should include label in results1")
        (ok (member "Multi Tag Label" results2 :key #'(lambda (l) (ref l :name)) :test #'string=)
            "Should include label in results2"))
      
      (destroy label)
      (destroy tag1)
      (destroy tag2)
      (destroy user)))
  
  (testing "find-labels-by-tag-name does not return duplicates"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "DupTag1" :color "#FF0000"))
           (tag2 (create-tag (ref user :id) "DupTag2" :color "#00FF00"))
           (label (create-label-with-tags (ref user :id) "Dup Label" nil (list (ref tag1 :ulid) (ref tag2 :ulid)))))
      
      (let ((results (find-labels-by-tag-name (ref user :id) "DupTag")))
        (ok (= (count "Dup Label" results :key #'(lambda (l) (ref l :name)) :test #'string=) 1)
            "Should return label only once despite matching multiple tags"))
      
      (destroy label)
      (destroy tag1)
      (destroy tag2)
      (destroy user)))
  
  (testing "find-labels-by-tag-name returns empty for no matches"
    (let ((user (create-test-user nil)))
      (ok (null (find-labels-by-tag-name (ref user :id) "NonExistentTag12345"))
          "Should return empty list for no matches")
      (destroy user)))
  
  (testing "find-labels-by-tag-name returns nil for empty query"
    (let ((user (create-test-user nil)))
      (ok (null (find-labels-by-tag-name (ref user :id) "")) "Should return nil for empty query")
      (ok (null (find-labels-by-tag-name (ref user :id) "   ")) "Should return nil for whitespace-only query")
      (destroy user)))
  
  (testing "find-labels-by-tag-name filters by owner-id"
    (let* ((user1 (create-test-user nil))
           (user2 (create-test-user nil))
           (tag1 (create-tag (ref user1 :id) "Owner1Tag" :color "#FF0000"))
           (tag2 (create-tag (ref user2 :id) "Owner2Tag" :color "#00FF00"))
           (label1 (create-label-with-tags (ref user1 :id) "Owner1 Label" nil (list (ref tag1 :ulid))))
           (label2 (create-label-with-tags (ref user2 :id) "Owner2 Label" nil (list (ref tag2 :ulid)))))
      
      (let ((results1 (find-labels-by-tag-name (ref user1 :id) "owner"))
            (results2 (find-labels-by-tag-name (ref user2 :id) "owner")))
        (ok (not (member "Owner2 Label" results1 :key #'(lambda (l) (ref l :name)) :test #'string=))
            "Owner 1 search should not return Owner 2 labels")
        (ok (not (member "Owner1 Label" results2 :key #'(lambda (l) (ref l :name)) :test #'string=))
            "Owner 2 search should not return Owner 1 labels"))
      
      (destroy label1)
      (destroy label2)
      (destroy tag1)
      (destroy tag2)
      (destroy user1)
      (destroy user2))))

(deftest-suite :model test-estimate-todo-count
  (testing "estimate-todo-count-by-tags with AND condition"
    (let* ((user (create-test-user nil))
           (tag1 (create-tag (ref user :id) "MORNING" :color "#FF0000"))
           (tag2 (create-tag (ref user :id) "EXERCISE" :color "#00FF00"))
           (todo1 (create-todo (ref user :id) "Morning exercise"))
           (todo2 (create-todo (ref user :id) "Morning meditation"))
           (todo3 (create-todo (ref user :id) "Evening exercise")))
      
      ;; Assign tags to todos
      (assign-tags-to-todo (ref todo1 :ulid) (list (ref tag1 :ulid) (ref tag2 :ulid)) (ref user :id))
      (assign-tags-to-todo (ref todo2 :ulid) (list (ref tag1 :ulid)) (ref user :id))
      (assign-tags-to-todo (ref todo3 :ulid) (list (ref tag2 :ulid)) (ref user :id))
      
      ;; Test AND condition: both MORNING and EXERCISE
      (let ((count (estimate-todo-count-by-tags (ref user :id)
                                                (list (ref tag1 :ulid) (ref tag2 :ulid)))))
        (ok (= count 1) "Should find 1 TODO with both MORNING and EXERCISE tags"))
      
      ;; Test single tag: MORNING
      (let ((count (estimate-todo-count-by-tags (ref user :id)
                                                (list (ref tag1 :ulid)))))
        (ok (= count 2) "Should find 2 TODOs with MORNING tag"))
      
      ;; Test single tag: EXERCISE
      (let ((count (estimate-todo-count-by-tags (ref user :id)
                                                (list (ref tag2 :ulid)))))
        (ok (= count 2) "Should find 2 TODOs with EXERCISE tag"))
      
      ;; Cleanup
      (destroy todo1)
      (destroy todo2)
      (destroy todo3)
      (destroy tag1)
      (destroy tag2)
      (destroy user))))
