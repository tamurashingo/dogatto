# Refactoring Plan: Service Layer Introduction

## Overview
This plan introduces a service layer between controllers and models to separate business logic from presentation and data layers.

## Current Architecture
- Controllers directly call model functions
- Business logic mixed with presentation logic in controllers
- No transaction management in controllers (except tag-merge)
- Models handle database operations and basic validation

## Target Architecture
- Controllers: Handle HTTP request/response, parameter parsing
- Services: Implement business logic, transaction management, call multiple models
- Models: Database operations, single-model validation

## Reference Implementation
- app/services/tag-merge-service.lisp demonstrates the target pattern

## Existing Controllers to Refactor
1. todos-controller.lisp
   - <todos-list-controller> (GET, POST)
   - <todo-item-controller> (GET, PUT, DELETE)
   - <todo-complete-controller> (PUT)

2. tags-controller.lisp
   - <tags-list-controller> (GET, POST)
   - <tag-item-controller> (GET, PUT, DELETE)

3. labels-controller.lisp
   - <labels-list-controller> (GET, POST)
   - <label-item-controller> (GET, PUT, DELETE)
   - <label-estimate-controller> (GET)

4. auth-controller.lisp
   - <auth-register-controller> (POST)
   - <auth-login-controller> (POST)
   - <auth-logout-controller> (POST)
   - <auth-me-controller> (GET)

5. todo-tags-controller.lisp
   - Need to examine

6. tags-merge-controller.lisp
   - Already uses service layer (reference implementation)

## Refactoring Tasks

### Phase 1: Create Service Layer Infrastructure
- [x] Reference: tag-merge-service.lisp exists
- [ ] Create app/services directory structure if not complete
- [ ] Define service naming conventions
- [ ] Document service layer patterns

### Phase 2: Authentication Services
- [ ] Create auth-service.lisp
  - register-user function
  - login-user function
  - logout-user function
  - get-current-user function
- [ ] Refactor auth-controller.lisp to use auth-service
- [ ] Test authentication flows

### Phase 3: TODO Services
- [ ] Create todo-service.lisp
  - list-todos function (with filtering by tags/label/status)
  - get-todo function
  - create-todo function
  - update-todo function
  - delete-todo function
  - toggle-todo-status function
  - Handle tag associations
- [ ] Refactor todos-controller.lisp to use todo-service
- [ ] Test TODO operations

### Phase 4: Tag Services
- [ ] Create tag-service.lisp
  - list-tags function
  - get-tag-with-stats function
  - create-tag function
  - update-tag function
  - delete-tag function
- [ ] Refactor tags-controller.lisp to use tag-service
- [ ] Test tag operations

### Phase 5: Label Services
- [ ] Create label-service.lisp
  - list-labels function (with search)
  - get-label function
  - create-label-with-tags function
  - update-label-with-tags function
  - delete-label function
  - estimate-todo-count function
- [ ] Refactor labels-controller.lisp to use label-service
- [ ] Test label operations

### Phase 6: TODO-Tag Association Services
- [ ] Examine todo-tags-controller.lisp
- [ ] Create todo-tag-service.lisp if needed
  - assign-tags function
  - remove-tags function
- [ ] Refactor todo-tags-controller.lisp if needed
- [ ] Test tag assignment operations

### Phase 7: Utility Refactoring
- [ ] Extract common authentication logic to helper
  - get-authenticated-user is duplicated in multiple controllers
  - Move to app/helpers or app/utils
- [ ] Extract common JSON conversion helpers
  - todo-to-json, tag-to-json, label-to-json
  - Create app/helpers/json-converters.lisp

### Phase 8: Integration Testing
- [ ] Run all existing tests
- [ ] Add service-level tests
- [ ] Verify transaction management
- [ ] Test error handling

## Service Layer Patterns (from tag-merge-service.lisp)

### Package Definition
```lisp
(defpackage #:dogatto/services/<service-name>
  (:use #:cl)
  (:import-from #:clails/model
                #:ref
                #:save
                #:execute-query
                #:with-transaction)
  (:import-from #:dogatto/models/...
                #:...)
  (:export #:function-name))
```

### Function Structure
- Validation first
- Return plist with :success, :errors, and data
- Use with-transaction for multi-model operations
- Clear error messages

### Transaction Management
```lisp
(with-transaction
  ;; Multiple model operations here
  ...)
```

## Controller Patterns After Refactoring

### Responsibilities
1. Extract authentication (get-authenticated-user)
2. Parse request parameters
3. Call service layer functions
4. Format HTTP response
5. Set appropriate HTTP status codes
6. NO business logic

### Example Pattern
```lisp
(defmethod do-post ((controller <some-controller>))
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (return-unauthorized controller))
    
    (let* ((param1 (param controller "param1"))
           (param2 (param controller "param2"))
           (result (service-function user param1 param2)))
      
      (if (getf result :success)
          (respond-success controller (getf result :data))
          (respond-error controller (getf result :errors))))))
```

## Model Layer After Refactoring

### Responsibilities
1. Database CRUD operations
2. Single-model validation
3. Simple queries
4. NO transaction management (handled by service)
5. NO multi-model operations

## Notes
- Keep existing model functions working
- Services add new layer, don't replace models
- Controllers become thin wrappers
- All business logic moves to services
- Transaction boundaries defined in services

