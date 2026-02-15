; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto-test/integration/label-search
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
                #:<label>)
  (:import-from #:dogatto/models/label-tag
                #:create-label-with-tags)
  (:import-from #:dogatto/models/todo
                #:create-todo
                #:find-todos-by-label)
  (:import-from #:dogatto/models/todo-tag
                #:assign-tags-to-todo)
  (:import-from #:dogatto/utils/ulid
                #:generate-ulid))
(in-package #:dogatto-test/integration/label-search)

(defun create-test-user (name)
  "Create a test user with generated ULID and unique email.

   @param name [string] User name (for identification in tests)
   @return [<user>] Created user instance
   "
  (let ((unique-email (format nil "test-~A@example.com" (generate-ulid))))
    (create-user :username name
                 :email unique-email
                 :password-hash "test-hash"
                 :ulid (generate-ulid))))

(deftest-suite :integration test-label-search-with-and-condition
  (testing "Label search with AND condition - should find TODO with all tags"
    (let* ((user (create-test-user "label-search-user"))
           (user-id (ref user :id))
           
           ;; Create tags: daily and morning
           (tag-daily (create-tag user-id "DAILY" :color "#FF0000"))
           (tag-morning (create-tag user-id "MORNING" :color "#00FF00"))
           
           ;; Create label: "morning routine" = [DAILY] AND [MORNING]
           (label-morning-routine (create-label-with-tags
                                   user-id
                                   "morning routine"
                                   "Daily morning routine tasks"
                                   (list (ref tag-daily :ulid)
                                         (ref tag-morning :ulid))))
           
           ;; Create TODOs
           (todo-a (create-todo user-id "TODO A" :content "Has only DAILY tag"))
           (todo-b (create-todo user-id "TODO B" :content "Has DAILY and MORNING tags"))
           (todo-c (create-todo user-id "TODO C" :content "Has no tags")))
      
      ;; Assign tags to TODOs
      ;; TODO A: [daily]
      (assign-tags-to-todo (ref todo-a :ulid)
                           (list (ref tag-daily :ulid))
                           user-id)
      
      ;; TODO B: [daily] [morning]
      (assign-tags-to-todo (ref todo-b :ulid)
                           (list (ref tag-daily :ulid)
                                 (ref tag-morning :ulid))
                           user-id)
      
      ;; TODO C: [] (no tags)
      ;; (nothing to do)
      
      ;; Test: Search by label "morning routine"
      ;; Expected: Should return only TODO B
      (let ((todos-by-label (find-todos-by-label (ref label-morning-routine :ulid) user-id)))
        (ok (= (length todos-by-label) 1)
            "Should find exactly 1 TODO with label 'morning routine'")
        
        (ok (member (ref todo-b :id)
                    todos-by-label
                    :key #'(lambda (todo) (ref todo :id)))
            "Should find TODO B (has both DAILY and MORNING tags)")
        
        (ok (not (member (ref todo-a :id)
                         todos-by-label
                         :key #'(lambda (todo) (ref todo :id))))
            "Should NOT find TODO A (has only DAILY tag)")
        
        (ok (not (member (ref todo-c :id)
                         todos-by-label
                         :key #'(lambda (todo) (ref todo :id))))
            "Should NOT find TODO C (has no tags)")
        
        ;; Verify the found TODO is indeed TODO B
        (when (> (length todos-by-label) 0)
          (let ((found-todo (first todos-by-label)))
            (ok (string= (ref found-todo :title) "TODO B")
                "Found TODO should be TODO B"))))
      
      ;; Cleanup
      (destroy todo-a)
      (destroy todo-b)
      (destroy todo-c)
      (destroy label-morning-routine)
      (destroy tag-daily)
      (destroy tag-morning)
      (destroy user))))

(deftest-suite :integration test-label-search-with-single-tag
  (testing "Label search with single tag - should find all TODOs with that tag"
    (let* ((user (create-test-user "single-tag-user"))
           (user-id (ref user :id))
           
           ;; Create tag: work
           (tag-work (create-tag user-id "WORK" :color "#0000FF"))
           
           ;; Create label: "work tasks" = [WORK]
           (label-work (create-label-with-tags
                        user-id
                        "work tasks"
                        "All work related tasks"
                        (list (ref tag-work :ulid))))
           
           ;; Create TODOs
           (todo-1 (create-todo user-id "Work TODO 1" :content "Work task 1"))
           (todo-2 (create-todo user-id "Work TODO 2" :content "Work task 2"))
           (todo-3 (create-todo user-id "Personal TODO" :content "Personal task")))
      
      ;; Assign tags
      (assign-tags-to-todo (ref todo-1 :ulid) (list (ref tag-work :ulid)) user-id)
      (assign-tags-to-todo (ref todo-2 :ulid) (list (ref tag-work :ulid)) user-id)
      ;; todo-3 has no tags
      
      ;; Test: Search by label "work tasks"
      (let ((todos-by-label (find-todos-by-label (ref label-work :ulid) user-id)))
        (ok (= (length todos-by-label) 2)
            "Should find exactly 2 TODOs with label 'work tasks'")
        
        (ok (member (ref todo-1 :id)
                    todos-by-label
                    :key #'(lambda (todo) (ref todo :id)))
            "Should find Work TODO 1")
        
        (ok (member (ref todo-2 :id)
                    todos-by-label
                    :key #'(lambda (todo) (ref todo :id)))
            "Should find Work TODO 2")
        
        (ok (not (member (ref todo-3 :id)
                         todos-by-label
                         :key #'(lambda (todo) (ref todo :id))))
            "Should NOT find Personal TODO (no WORK tag)"))
      
      ;; Cleanup
      (destroy todo-1)
      (destroy todo-2)
      (destroy todo-3)
      (destroy label-work)
      (destroy tag-work)
      (destroy user))))

(deftest-suite :integration test-label-search-with-three-tags
  (testing "Label search with three tags - AND condition"
    (let* ((user (create-test-user "three-tags-user"))
           (user-id (ref user :id))
           
           ;; Create tags
           (tag-urgent (create-tag user-id "URGENT" :color "#FF0000"))
           (tag-important (create-tag user-id "IMPORTANT" :color "#FF6600"))
           (tag-today (create-tag user-id "TODAY" :color "#FFCC00"))
           
           ;; Create label: "critical tasks" = [URGENT] AND [IMPORTANT] AND [TODAY]
           (label-critical (create-label-with-tags
                            user-id
                            "critical tasks"
                            "Urgent, important, and due today"
                            (list (ref tag-urgent :ulid)
                                  (ref tag-important :ulid)
                                  (ref tag-today :ulid))))
           
           ;; Create TODOs
           (todo-all-tags (create-todo user-id "Critical Task" :content "Has all 3 tags"))
           (todo-two-tags (create-todo user-id "Important Task" :content "Has 2 tags"))
           (todo-one-tag (create-todo user-id "Urgent Task" :content "Has 1 tag"))
           (todo-no-tags (create-todo user-id "Normal Task" :content "No tags")))
      
      ;; Assign tags
      (assign-tags-to-todo (ref todo-all-tags :ulid)
                           (list (ref tag-urgent :ulid)
                                 (ref tag-important :ulid)
                                 (ref tag-today :ulid))
                           user-id)
      
      (assign-tags-to-todo (ref todo-two-tags :ulid)
                           (list (ref tag-urgent :ulid)
                                 (ref tag-important :ulid))
                           user-id)
      
      (assign-tags-to-todo (ref todo-one-tag :ulid)
                           (list (ref tag-urgent :ulid))
                           user-id)
      
      ;; Test: Search by label "critical tasks"
      (let ((todos-by-label (find-todos-by-label (ref label-critical :ulid) user-id)))
        (ok (= (length todos-by-label) 1)
            "Should find exactly 1 TODO with all 3 tags")
        
        (ok (member (ref todo-all-tags :id)
                    todos-by-label
                    :key #'(lambda (todo) (ref todo :id)))
            "Should find Critical Task (has all 3 tags)")
        
        (ok (not (member (ref todo-two-tags :id)
                         todos-by-label
                         :key #'(lambda (todo) (ref todo :id))))
            "Should NOT find Important Task (missing TODAY tag)")
        
        (ok (not (member (ref todo-one-tag :id)
                         todos-by-label
                         :key #'(lambda (todo) (ref todo :id))))
            "Should NOT find Urgent Task (missing IMPORTANT and TODAY tags)"))
      
      ;; Cleanup
      (destroy todo-all-tags)
      (destroy todo-two-tags)
      (destroy todo-one-tag)
      (destroy todo-no-tags)
      (destroy label-critical)
      (destroy tag-urgent)
      (destroy tag-important)
      (destroy tag-today)
      (destroy user))))

(deftest-suite :integration test-label-search-empty-results
  (testing "Label search returns empty when no TODOs match"
    (let* ((user (create-test-user "empty-result-user"))
           (user-id (ref user :id))
           
           ;; Create tags
           (tag-special (create-tag user-id "SPECIAL" :color "#FF00FF"))
           (tag-rare (create-tag user-id "RARE" :color "#00FFFF"))
           
           ;; Create label with tags that no TODO has
           (label-rare (create-label-with-tags
                        user-id
                        "rare combination"
                        "A combination that doesn't exist"
                        (list (ref tag-special :ulid)
                              (ref tag-rare :ulid))))
           
           ;; Create TODO with only one of the tags
           (todo-partial (create-todo user-id "Partial Match" :content "Has only SPECIAL tag")))
      
      (assign-tags-to-todo (ref todo-partial :ulid)
                           (list (ref tag-special :ulid))
                           user-id)
      
      ;; Test: Search by label should return empty
      (let ((todos-by-label (find-todos-by-label (ref label-rare :ulid) user-id)))
        (ok (= (length todos-by-label) 0)
            "Should return 0 TODOs when no TODO has all required tags"))
      
      ;; Cleanup
      (destroy todo-partial)
      (destroy label-rare)
      (destroy tag-special)
      (destroy tag-rare)
      (destroy user))))
