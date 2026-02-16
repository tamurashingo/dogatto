# Service Layer Patterns

## Overview
This document describes the standard patterns for implementing services in the dogatto application. All services should follow these patterns for consistency and maintainability.

## Reference Implementation
The `app/services/tag-merge-service.lisp` serves as the reference implementation demonstrating these patterns.

---

## 1. Package Structure

### Package Definition
Services use the `package-inferred-system` and follow this structure:

```common-lisp
; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/services/<service-name>
  (:use #:cl)
  (:import-from #:clails/model
                #:ref
                #:save
                #:execute-query
                #:with-transaction)
  (:import-from #:dogatto/models/...
                #:<model-class>
                #:model-function)
  (:import-from #:dogatto/utils/...
                #:utility-function)
  (:export #:service-function-1
           #:service-function-2))

(in-package #:dogatto/services/<service-name>)
```

### Key Points
- Use `#:` for keyword symbols (no `use` except for `#:cl`)
- Import specific symbols from models and utilities using `import-from`
- Export only public service functions
- File naming: `<domain>-service.lisp` (e.g., `auth-service.lisp`, `todo-service.lisp`)

---

## 2. Function Signature Patterns

### Standard Return Format
All service functions MUST return a plist with the following structure:

#### Success Response
```common-lisp
(list :success t
      :data data-value)
```

Or with multiple data fields:
```common-lisp
(list :success t
      :user user-instance
      :session-id session-id-value)
```

#### Error Response
```common-lisp
(list :success nil
      :errors error-list)
```

Where `error-list` is a list of error message strings:
```common-lisp
'("Error message 1" "Error message 2")
```

### Examples

#### Simple Success
```common-lisp
(defun get-todo (todo-ulid owner-id)
  "Get a TODO by ULID for the specified owner.
   
   @param todo-ulid [string] TODO ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success with :todo or error with :errors
   "
  (let ((todo (find-todo-by-ulid todo-ulid owner-id)))
    (if todo
        (list :success t :todo todo)
        (list :success nil :errors '("TODO not found")))))
```

#### Validation with Multiple Errors
```common-lisp
(defun create-todo (owner-id title &key content due-date)
  "Create a new TODO.
   
   @param owner-id [integer] Owner ID
   @param title [string] TODO title
   @param content [string] TODO content (optional)
   @param due-date [integer] Due date as universal time (optional)
   @return [plist] Success with :todo or error with :errors
   "
  (let ((errors '()))
    ;; Validate
    (when (or (null title) (string= (string-trim '(#\Space) title) ""))
      (push "Title is required" errors))
    (when (and title (> (length title) 200))
      (push "Title must be 200 characters or less" errors))
    
    ;; Return errors if any
    (when errors
      (return-from create-todo
        (list :success nil :errors (nreverse errors))))
    
    ;; Create and return
    (let ((todo (dogatto/models/todo:create-todo owner-id title
                                                  :content content
                                                  :due-date due-date)))
      (list :success t :todo todo))))
```

---

## 3. Validation Patterns

### Input Validation
Always validate inputs at the service layer before calling models.

```common-lisp
(defun some-service-function (param1 param2 owner-id)
  (let ((errors '()))
    
    ;; Required field validation
    (when (null param1)
      (push "Parameter 1 is required" errors))
    
    ;; Format validation
    (when (and param1 (not (stringp param1)))
      (push "Parameter 1 must be a string" errors))
    
    ;; Length validation
    (when (and param1 (> (length param1) 100))
      (push "Parameter 1 must be 100 characters or less" errors))
    
    ;; Return errors early
    (when errors
      (return-from some-service-function
        (list :success nil :errors (nreverse errors))))
    
    ;; Continue with business logic
    ...))
```

### Authorization Validation
Verify ownership and permissions:

```common-lisp
(defun update-todo (todo-ulid owner-id &key title content)
  (let ((todo (find-todo-by-ulid todo-ulid)))
    
    ;; Check existence
    (unless todo
      (return-from update-todo
        (list :success nil :errors '("TODO not found"))))
    
    ;; Check ownership
    (unless (= (ref todo :owner-id) owner-id)
      (return-from update-todo
        (list :success nil :errors '("Unauthorized"))))
    
    ;; Continue with update
    ...))
```

### Model Validation
Let models validate their own constraints:

```common-lisp
(defun create-tag (owner-id name &key color)
  ;; Service layer validates business rules
  (let ((errors '()))
    (when (null name)
      (push "Name is required" errors))
    (when errors
      (return-from create-tag
        (list :success nil :errors errors)))
    
    ;; Model validates data constraints (uniqueness, format, etc.)
    (handler-case
        (let ((tag (dogatto/models/tag:create-tag owner-id name :color color)))
          (list :success t :tag tag))
      (error (e)
        (list :success nil :errors (list (format nil "~A" e)))))))
```

---

## 4. Transaction Management Patterns

### Single Transaction
Use `with-transaction` for operations that modify multiple models:

```common-lisp
(defun merge-tags-to-existing (source-ulids target-ulid owner-id)
  "Merge multiple source tags into an existing target tag.
   
   @param source-ulids [list] List of source tag ULIDs
   @param target-ulid [string] Target tag ULID
   @param owner-id [integer] Owner ID
   @return [plist] Success with :merged-tags, :target-tag or error with :errors
   "
  ;; Validate first (outside transaction)
  (let ((errors (validate-inputs source-ulids target-ulid owner-id)))
    (when errors
      (return-from merge-tags-to-existing
        (list :success nil :errors errors))))
  
  ;; Execute in transaction
  (with-transaction
    (let ((merged-tags '())
          (target-tag (find-tag-by-ulid target-ulid owner-id)))
      
      (dolist (source-ulid source-ulids)
        (let ((source-tag (find-tag-by-ulid source-ulid owner-id)))
          ;; Copy associations
          (copy-todo-tags-for-merge (ref source-tag :id) (ref target-tag :id))
          (copy-label-tags-for-merge (ref source-tag :id) (ref target-tag :id))
          
          ;; Delete old associations
          (delete-todo-tags-for-merge (ref source-tag :id))
          (delete-label-tags-for-merge (ref source-tag :id))
          
          ;; Mark as merged
          (setf (ref source-tag :merged-to-ulid) target-ulid)
          (save source-tag)
          
          (push source-tag merged-tags)))
      
      (list :success t
            :merged-tags (nreverse merged-tags)
            :target-tag target-tag))))
```

### Transaction Guidelines
1. **Validate before transaction**: Perform all validation outside the transaction
2. **Keep transactions short**: Only database operations inside transaction
3. **All or nothing**: All operations in transaction succeed or all rollback
4. **Error handling**: Errors inside `with-transaction` automatically rollback

### When to Use Transactions
- Creating/updating multiple related records
- Deleting records with manual cascade operations
- Moving associations between entities
- Any operation where partial completion would leave inconsistent state

### When NOT to Use Transactions
- Single model CRUD operations (model handles this)
- Read-only operations
- Operations without database modifications

---

## 5. Error Handling

### Service Layer Error Handling
```common-lisp
(defun some-operation (param)
  ;; Wrap model calls that might throw errors
  (handler-case
      (let ((result (model-operation param)))
        (list :success t :result result))
    (model-validation-error (e)
      (list :success nil :errors (list (format nil "Validation failed: ~A" e))))
    (database-error (e)
      (list :success nil :errors (list (format nil "Database error: ~A" e))))
    (error (e)
      (list :success nil :errors (list (format nil "Unexpected error: ~A" e))))))
```

### Error Message Guidelines
1. **Be specific**: "TODO not found" not "Error"
2. **Be helpful**: "Tag name must be 50 characters or less" not "Invalid name"
3. **Don't expose internals**: Don't include stack traces or SQL in user-facing errors
4. **Consistent format**: Use sentence case, end with period if full sentence

---

## 6. Documentation Standards

### Docstring Format
Follow the AGENTS.md conventions:

```common-lisp
(defun service-function (param1 param2 &key optional-param)
  "Brief one-sentence description of what the function does.
   
   Detailed description if needed. Can span multiple lines.
   Explain business logic, transaction boundaries, special cases.
   
   @param param1 [type] Description of param1
   @param param2 [type] Description of param2
   @param optional-param [type] Description of optional parameter
   @return [plist] Success with :field1, :field2 or error with :errors
   @condition error-type When this error might be thrown
   "
  ...)
```

### Code Comments
- Comment WHY not WHAT
- Explain business rules
- Note transaction boundaries
- Highlight edge cases

---

## 7. Service Organization

### File Structure
```
app/services/
├── tag-merge-service.lisp      # Reference implementation
├── auth-service.lisp           # Authentication operations
├── todo-service.lisp           # TODO CRUD and business logic
├── tag-service.lisp            # Tag management
├── label-service.lisp          # Label management
└── todo-tag-service.lisp       # TODO-Tag associations (if needed)
```

### Function Organization Within Service
1. Public API functions (exported)
2. Private helper functions (not exported)
3. Validation functions
4. Data transformation functions

### Naming Conventions
- **Service files**: `<domain>-service.lisp`
- **Service functions**: `<verb>-<noun>` (e.g., `create-todo`, `list-tags`)
- **Validation functions**: `validate-<what>` (e.g., `validate-todo-input`)
- **Helper functions**: descriptive names (e.g., `fetch-todo-with-tags`)

---

## 8. Integration with Controllers

### Controller Responsibilities
Controllers should ONLY:
1. Extract and parse request parameters
2. Get authenticated user
3. Call service function
4. Format HTTP response
5. Set HTTP status code

### Anti-Pattern (Don't Do This)
```common-lisp
(defmethod do-post ((controller <todos-list-controller>))
  (let ((user (get-authenticated-user (env controller))))
    ;; DON'T: Business logic in controller
    (let ((title (param controller "title")))
      (when (null title)
        (return-from do-post ...))
      (let ((todo (create-todo (ref user :id) title)))
        ...))))
```

### Correct Pattern
```common-lisp
(defmethod do-post ((controller <todos-list-controller>))
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'code) 401)
      (return-from do-post
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))
    
    ;; Extract parameters
    (let* ((title (param controller "title"))
           (content (param controller "content"))
           (due-date (param controller "dueDate"))
           ;; Call service
           (result (todo-service:create-todo (ref user :id) title
                                             :content content
                                             :due-date due-date)))
      
      ;; Format response
      (if (getf result :success)
          (progn
            (setf (slot-value controller 'code) 201)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("todo" . ,(todo-to-json (getf result :todo))))))))
          (progn
            (setf (slot-value controller 'code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))
```

---

## 9. Testing Services

### Test Structure
```common-lisp
(deftest test-create-todo-success
  (testing "create-todo with valid parameters"
    (let* ((user (create-test-user))
           (result (todo-service:create-todo (ref user :id) "Test TODO")))
      (ok (getf result :success))
      (ok (getf result :todo))
      (ok (string= (ref (getf result :todo) :title) "Test TODO")))))

(deftest test-create-todo-validation-error
  (testing "create-todo with missing title"
    (let* ((user (create-test-user))
           (result (todo-service:create-todo (ref user :id) nil)))
      (ng (getf result :success))
      (ok (getf result :errors))
      (ok (member "Title is required" (getf result :errors) :test #'string=)))))

(deftest test-create-todo-unauthorized
  (testing "create-todo with wrong owner"
    (let* ((user1 (create-test-user))
           (user2 (create-test-user))
           (todo (create-test-todo (ref user1 :id)))
           (result (todo-service:update-todo (ref todo :ulid) (ref user2 :id)
                                            :title "Hacked")))
      (ng (getf result :success))
      (ok (member "Unauthorized" (getf result :errors) :test #'string=)))))
```

### Test Coverage
- Success cases
- Validation errors
- Authorization errors
- Transaction rollback scenarios
- Edge cases

---

## 10. Common Patterns Summary

### Checklist for New Services
- [ ] Package definition with proper imports
- [ ] All functions return plist with `:success` and either `:errors` or data
- [ ] Validation before business logic
- [ ] Authorization checks where needed
- [ ] Transactions for multi-model operations
- [ ] Proper docstrings on all functions
- [ ] Error handling with descriptive messages
- [ ] Unit tests for all functions
- [ ] Export only public API functions

### Code Review Checklist
- [ ] No business logic in controllers
- [ ] Transactions used appropriately
- [ ] Consistent error format
- [ ] No model logic in services (only orchestration)
- [ ] Proper validation and authorization
- [ ] Tests cover success and error cases
