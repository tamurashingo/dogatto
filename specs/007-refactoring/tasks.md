# Task List: Service Layer Refactoring

## Overview
Break down the service layer refactoring into small, implementable tasks with clear dependencies and completion criteria.

---

## Phase 0: Preparation and Infrastructure

### Task 0.1: Document service layer patterns ✅
**Status**: Completed (Commit: 1ea9fb9)  
**Priority**: High  
**Depends on**: None  
**Estimated effort**: 1 hour  
**Description**: Create service-patterns.md documenting standard patterns from tag-merge-service.lisp
**Completion criteria**:
- [x] Document package structure
- [x] Document function signature patterns (return plist with :success, :errors, :data)
- [x] Document transaction management patterns
- [x] Document validation patterns

### Task 0.2: Create common helper for authentication ✅
**Status**: Completed (Commit: 1ea9fb9)  
**Priority**: High  
**Depends on**: None  
**Estimated effort**: 2 hours  
**Description**: Extract get-authenticated-user from controllers into app/helpers/auth-helper.lisp
**Completion criteria**:
- [x] Create app/helpers/auth-helper.lisp
- [x] Export get-authenticated-user function
- [x] Function extracts session_id from cookie
- [x] Function validates session and returns user
- [x] Keep duplicate code in controllers temporarily (remove in later phase)

### Task 0.3: Create common helper for JSON conversion ✅
**Status**: Completed (Commit: 1ea9fb9)  
**Priority**: Medium  
**Depends on**: None  
**Estimated effort**: 2 hours  
**Description**: Extract JSON conversion functions into app/helpers/json-converters.lisp
**Completion criteria**:
- [x] Create app/helpers/json-converters.lisp
- [x] Extract todo-to-json function
- [x] Extract tag-to-json function
- [x] Extract label-to-json function
- [x] Extract user-to-json function
- [x] Keep duplicate code in controllers temporarily (remove in later phase)

---

## Phase 1: Authentication Services

### Task 1.1: Create auth-service skeleton ✅
**Status**: Completed (Commit: 885184a)  
**Priority**: High  
**Depends on**: Task 0.1  
**Estimated effort**: 1 hour  
**Description**: Create app/services/auth-service.lisp with package definition
**Completion criteria**:
- [x] File created with proper package definition
- [x] Import necessary model and util packages
- [x] Export function names (stub implementations)

### Task 1.2: Implement register-user function ✅
**Status**: Completed (Commit: 885184a)  
**Priority**: High  
**Depends on**: Task 1.1  
**Estimated effort**: 3 hours  
**Description**: Implement user registration logic in auth-service
**Completion criteria**:
- [x] Validate input parameters (email, username, password)
- [x] Check if user already exists
- [x] Hash password
- [x] Create user record
- [x] Return plist with :success, :user or :errors
- [x] Add docstring following AGENTS.md conventions

### Task 1.3: Implement login-user function ✅
**Status**: Completed (Commit: 885184a)  
**Priority**: High  
**Depends on**: Task 1.1  
**Estimated effort**: 2 hours  
**Description**: Implement user login logic in auth-service
**Completion criteria**:
- [x] Validate input parameters
- [x] Find user by email
- [x] Verify password
- [x] Create session
- [x] Return plist with :success, :session-id, :user or :errors
- [x] Add docstring

### Task 1.4: Implement logout-user function ✅
**Status**: Completed (Commit: 885184a)  
**Priority**: Medium  
**Depends on**: Task 1.1  
**Estimated effort**: 1 hour  
**Description**: Implement user logout logic in auth-service
**Completion criteria**:
- [x] Validate session-id
- [x] Delete session
- [x] Return plist with :success or :errors
- [x] Add docstring

### Task 1.5: Implement get-current-user function ✅
**Status**: Completed (Commit: 885184a)  
**Priority**: Medium  
**Depends on**: Task 1.1  
**Estimated effort**: 1 hour  
**Description**: Implement current user retrieval logic in auth-service
**Completion criteria**:
- [x] Validate session-id
- [x] Get session data
- [x] Find user by id
- [x] Return plist with :success, :user or :errors
- [x] Add docstring

### Task 1.6: Write tests for auth-service ✅
**Status**: Completed (Commit: 885184a)  
**Priority**: High  
**Depends on**: Task 1.2, 1.3, 1.4, 1.5  
**Estimated effort**: 3 hours  
**Description**: Create comprehensive tests for auth-service functions
**Completion criteria**:
- [x] Test register-user (success, duplicate email, validation errors)
- [x] Test login-user (success, wrong password, user not found)
- [x] Test logout-user (success, invalid session)
- [x] Test get-current-user (success, invalid session)
- [x] All tests pass

### Task 1.7: Refactor auth-controller to use auth-service ✅
**Status**: Completed (Commit: 885184a)  
**Priority**: High  
**Depends on**: Task 1.6  
**Estimated effort**: 3 hours  
**Description**: Update auth-controller.lisp to delegate to auth-service
**Completion criteria**:
- [x] <auth-register-controller> calls register-user
- [x] <auth-login-controller> calls login-user
- [x] <auth-logout-controller> calls logout-user
- [x] <auth-me-controller> calls get-current-user
- [x] Controllers only handle HTTP concerns
- [x] All authentication tests still pass

---

## Phase 2: TODO Services

### Task 2.1: Create todo-service skeleton ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: High  
**Depends on**: Task 0.1, 1.7  
**Estimated effort**: 1 hour  
**Description**: Create app/services/todo-service.lisp with package definition
**Completion criteria**:
- [x] File created with proper package definition
- [x] Import necessary model packages
- [x] Export function names (stub implementations)

### Task 2.2: Implement list-todos function ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: High  
**Depends on**: Task 2.1  
**Estimated effort**: 3 hours  
**Description**: Implement TODO listing with filtering logic
**Completion criteria**:
- [x] Handle filtering by tag-ulids (OR condition)
- [x] Handle filtering by label-ulid (AND condition on label's tags)
- [x] Handle filtering by status (active/completed)
- [x] Handle untagged filter
- [x] Fetch associated tags for each TODO
- [x] Return plist with :success, :todos or :errors
- [x] Add docstring

### Task 2.3: Implement get-todo function ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: High  
**Depends on**: Task 2.1  
**Estimated effort**: 2 hours  
**Description**: Implement single TODO retrieval logic
**Completion criteria**:
- [x] Validate todo-ulid and owner-id
- [x] Find TODO by ULID
- [x] Verify ownership
- [x] Fetch associated tags
- [x] Return plist with :success, :todo or :errors
- [x] Add docstring

### Task 2.4: Implement create-todo function ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: High  
**Depends on**: Task 2.1  
**Estimated effort**: 2 hours  
**Description**: Implement TODO creation logic
**Completion criteria**:
- [x] Validate input (title required, due-date format)
- [x] Create TODO record
- [x] Return plist with :success, :todo or :errors
- [x] Add docstring

### Task 2.5: Implement update-todo function ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: High  
**Depends on**: Task 2.1  
**Estimated effort**: 2 hours  
**Description**: Implement TODO update logic
**Completion criteria**:
- [x] Validate todo-ulid and owner-id
- [x] Find and verify ownership
- [x] Validate update parameters
- [x] Update TODO record
- [x] Return plist with :success, :todo or :errors
- [x] Add docstring

### Task 2.6: Implement delete-todo function ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: High  
**Depends on**: Task 2.1  
**Estimated effort**: 2 hours  
**Description**: Implement TODO deletion logic
**Completion criteria**:
- [x] Validate todo-ulid and owner-id
- [x] Find and verify ownership
- [x] Delete TODO (cascading to todo_tags handled by model)
- [x] Return plist with :success or :errors
- [x] Add docstring

### Task 2.7: Implement toggle-todo-status function ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: Medium  
**Depends on**: Task 2.1  
**Estimated effort**: 2 hours  
**Description**: Implement TODO status toggle logic
**Completion criteria**:
- [x] Validate todo-ulid and owner-id
- [x] Find and verify ownership
- [x] Toggle status between pending/completed
- [x] Update completed_at timestamp
- [x] Return plist with :success, :todo or :errors
- [x] Add docstring

### Task 2.8: Write tests for todo-service ⚠️
**Status**: Deferred (Existing controller tests provide coverage)  
**Priority**: High  
**Depends on**: Task 2.2, 2.3, 2.4, 2.5, 2.6, 2.7  
**Estimated effort**: 4 hours  
**Description**: Create comprehensive tests for todo-service functions
**Completion criteria**:
- [ ] Test all functions with success cases
- [ ] Test authorization failures
- [ ] Test validation errors
- [ ] Test filtering logic in list-todos
- [ ] All tests pass

### Task 2.9: Refactor todos-controller to use todo-service ✅
**Status**: Completed (Commit: 995764d)  
**Priority**: High  
**Depends on**: Task 2.8  
**Estimated effort**: 4 hours  
**Description**: Update todos-controller.lisp to delegate to todo-service
**Completion criteria**:
- [x] <todos-list-controller> GET calls list-todos
- [x] <todos-list-controller> POST calls create-new-todo
- [x] <todo-item-controller> GET calls get-todo
- [x] <todo-item-controller> PUT calls update-existing-todo
- [x] <todo-item-controller> DELETE calls delete-existing-todo
- [x] <todo-complete-controller> PUT calls toggle-todo-complete
- [x] Controllers only handle HTTP concerns
- [x] All TODO tests still pass

---

## Phase 3: Tag Services

### Task 3.1: Create tag-service skeleton ✅
**Status**: Completed (Commit: e7851f7)  
**Priority**: High  
**Depends on**: Task 0.1, 2.9  
**Estimated effort**: 1 hour  
**Description**: Create app/services/tag-service.lisp with package definition
**Completion criteria**:
- [x] File created with proper package definition
- [x] Import necessary model packages
- [x] Export function names (stub implementations)

### Task 3.2: Implement list-tags function ✅
**Status**: Completed (Commit: e7851f7)  
**Priority**: High  
**Depends on**: Task 3.1  
**Estimated effort**: 2 hours  
**Description**: Implement tag listing logic
**Completion criteria**:
- [x] Fetch all tags for user
- [x] Filter out merged tags (handled by model)
- [x] Return plist with :success, :tags or :errors
- [x] Add docstring

### Task 3.3: Implement get-tag-with-stats function ✅
**Status**: Completed (Commit: e7851f7)  
**Priority**: High  
**Depends on**: Task 3.1  
**Estimated effort**: 2 hours  
**Description**: Implement tag retrieval with statistics
**Completion criteria**:
- [x] Validate tag-ulid and owner-id
- [x] Find tag and verify ownership
- [x] Get tag usage statistics (active/completed TODO counts)
- [x] Return plist with :success, :tag, :statistics or :errors
- [x] Add docstring

### Task 3.4: Implement create-tag function ✅
**Status**: Completed (Commit: e7851f7)  
**Priority**: High  
**Depends on**: Task 3.1  
**Estimated effort**: 2 hours  
**Description**: Implement tag creation logic
**Completion criteria**:
- [x] Validate input (name required, color optional with default)
- [x] Check for duplicate tag names (handled by model)
- [x] Create tag record
- [x] Return plist with :success, :tag or :errors
- [x] Add docstring

### Task 3.5: Implement update-tag function ✅
**Status**: Completed (Commit: e7851f7)  
**Priority**: High  
**Depends on**: Task 3.1  
**Estimated effort**: 2 hours  
**Description**: Implement tag update logic
**Completion criteria**:
- [x] Validate tag-ulid and owner-id
- [x] Find tag and verify ownership
- [x] Validate update parameters
- [x] Update tag record
- [x] Return plist with :success, :tag or :errors
- [x] Add docstring

### Task 3.6: Implement delete-tag function ✅
**Status**: Completed (Commit: e7851f7)  
**Priority**: Medium  
**Depends on**: Task 3.1  
**Estimated effort**: 2 hours  
**Description**: Implement tag deletion logic
**Completion criteria**:
- [x] Validate tag-ulid and owner-id
- [x] Find tag and verify ownership
- [x] Delete tag (cascading handled by model)
- [x] Return plist with :success or :errors
- [x] Add docstring

### Task 3.7: Write tests for tag-service ⚠️
**Status**: Deferred (Existing controller tests provide coverage)  
**Priority**: High  
**Depends on**: Task 3.2, 3.3, 3.4, 3.5, 3.6  
**Estimated effort**: 3 hours  
**Description**: Create comprehensive tests for tag-service functions
**Completion criteria**:
- [ ] Test all functions with success cases
- [ ] Test authorization failures
- [ ] Test validation errors
- [ ] All tests pass

### Task 3.8: Refactor tags-controller to use tag-service ✅
**Status**: Completed (Commit: e7851f7)  
**Priority**: High  
**Depends on**: Task 3.7  
**Estimated effort**: 3 hours  
**Description**: Update tags-controller.lisp to delegate to tag-service
**Completion criteria**:
- [x] <tags-list-controller> GET calls list-tags
- [x] <tags-list-controller> POST calls create-new-tag
- [x] <tag-item-controller> GET calls get-tag-with-stats
- [x] <tag-item-controller> PUT calls update-existing-tag
- [x] <tag-item-controller> DELETE calls delete-existing-tag
- [x] Controllers only handle HTTP concerns
- [x] All tag tests still pass
- Controllers only handle HTTP concerns
- All tag tests still pass

---

## Phase 4: TODO-Tag Association Services

### Task 4.1: Analyze todo-tags-controller requirements ✅
**Status**: Completed (Commit: ffe21bd)  
**Priority**: Medium  
**Depends on**: Task 2.9, 3.8  
**Estimated effort**: 1 hour  
**Description**: Review todo-tags-controller.lisp to determine service needs
**Completion criteria**:
- [x] Document current operations (assign, remove)
- [x] Identify business logic to extract
- [x] Decided to integrate into todo-service instead of separate service

### Task 4.2: Implement assign-tags-to-todo function ✅
**Status**: Completed (Commit: ffe21bd)  
**Priority**: Medium  
**Depends on**: Task 4.1  
**Estimated effort**: 3 hours  
**Description**: Implement tag assignment logic in todo-service
**Completion criteria**:
- [x] Validate todo-ulid, tag-ulids, and owner-id
- [x] Verify TODO ownership
- [x] Verify all tags exist and belong to user (handled by model)
- [x] Remove existing tag associations (handled by model)
- [x] Create new tag associations (handled by model)
- [x] Use transaction for atomicity (handled by model)
- [x] Return plist with :success, :tags or :errors
- [x] Add docstring

### Task 4.3: Implement remove-tag-from-todo function ✅
**Status**: Completed (Commit: ffe21bd)  
**Priority**: Low  
**Depends on**: Task 4.1  
**Estimated effort**: 2 hours  
**Description**: Implement single tag removal logic
**Completion criteria**:
- [x] Validate todo-ulid, tag-ulid, and owner-id
- [x] Verify TODO ownership
- [x] Remove tag association
- [x] Return plist with :success or :errors
- [x] Add docstring

### Task 4.4: Write tests for todo-tag service functions ⚠️
**Status**: Deferred (Existing controller tests provide coverage)  
**Priority**: Medium  
**Depends on**: Task 4.2, 4.3  
**Estimated effort**: 2 hours  
**Description**: Create tests for tag association functions
**Completion criteria**:
- [ ] Test assign-tags-to-todo (success, validation, authorization)
- [ ] Test remove-tag-from-todo (success, not found)
- [ ] All tests pass

### Task 4.5: Refactor todo-tags-controller to use service ✅
**Status**: Completed (Commit: ffe21bd)  
**Priority**: Medium  
**Depends on**: Task 4.4  
**Estimated effort**: 2 hours  
**Description**: Update todo-tags-controller.lisp to delegate to service
**Completion criteria**:
- [x] GET calls get-todo-tags
- [x] PUT calls assign-todo-tags
- [x] DELETE calls remove-todo-tag
- [x] Controller only handles HTTP concerns
- [x] All tests still pass

---

## Phase 5: Label Services

### Task 5.1: Create label-service skeleton
**Priority**: Medium  
**Depends on**: Task 0.1, 4.5  
**Estimated effort**: 1 hour  
**Description**: Create app/services/label-service.lisp with package definition
**Completion criteria**:
- File created with proper package definition
- Import necessary model packages
- Export function names (stub implementations)

### Task 5.2: Implement list-labels function
**Priority**: Medium  
**Depends on**: Task 5.1  
**Estimated effort**: 2 hours  
**Description**: Implement label listing with search logic
**Completion criteria**:
- Support search by name parameter
- Support search by tag name
- Fetch tag count and TODO count for each label
- Return plist with :success, :labels or :errors
- Add docstring

### Task 5.3: Implement get-label function
**Priority**: Medium  
**Depends on**: Task 5.1  
**Estimated effort**: 2 hours  
**Description**: Implement single label retrieval logic
**Completion criteria**:
- Validate label-ulid and owner-id
- Find label and verify ownership
- Fetch associated tags
- Get statistics (tag count, TODO count)
- Return plist with :success, :label, :tags, :statistics or :errors
- Add docstring

### Task 5.4: Implement create-label-with-tags function
**Priority**: Medium  
**Depends on**: Task 5.1  
**Estimated effort**: 3 hours  
**Description**: Implement label creation with tag assignment
**Completion criteria**:
- Validate input (name required, tag-ulids list)
- Verify all tags exist and belong to user
- Create label record
- Assign tags to label
- Use transaction for atomicity
- Return plist with :success, :label, :tags or :errors
- Add docstring

### Task 5.5: Implement update-label-with-tags function
**Priority**: Medium  
**Depends on**: Task 5.1  
**Estimated effort**: 3 hours  
**Description**: Implement label update with tag reassignment
**Completion criteria**:
- Validate label-ulid and owner-id
- Find label and verify ownership
- Update label name if provided
- Update tag associations if provided
- Use transaction for atomicity
- Return plist with :success, :label, :tags or :errors
- Add docstring

### Task 5.6: Implement delete-label function
**Priority**: Low  
**Depends on**: Task 5.1  
**Estimated effort**: 2 hours  
**Description**: Implement label deletion logic
**Completion criteria**:
- Validate label-ulid and owner-id
- Find label and verify ownership
- Delete label (cascading handled by model)
- Return plist with :success or :errors
- Add docstring

### Task 5.7: Implement estimate-todo-count function
**Priority**: Low  
**Depends on**: Task 5.1  
**Estimated effort**: 2 hours  
**Description**: Implement TODO count estimation logic
**Completion criteria**:
- Validate tag-ulids and owner-id
- Calculate TODO count matching all tags (AND condition)
- Return plist with :success, :count or :errors
- Add docstring

### Task 5.8: Write tests for label-service
**Priority**: Medium  
**Depends on**: Task 5.2, 5.3, 5.4, 5.5, 5.6, 5.7  
**Estimated effort**: 4 hours  
**Description**: Create comprehensive tests for label-service functions
**Completion criteria**:
- Test all functions with success cases
- Test authorization failures
- Test validation errors
- Test transaction rollback scenarios
- All tests pass

### Task 5.9: Refactor labels-controller to use label-service
**Priority**: Medium  
**Depends on**: Task 5.8  
**Estimated effort**: 4 hours  
**Description**: Update labels-controller.lisp to delegate to label-service
**Completion criteria**:
- <labels-list-controller> GET calls list-labels
- <labels-list-controller> POST calls create-label-with-tags
- <label-item-controller> GET calls get-label
- <label-item-controller> PUT calls update-label-with-tags
- <label-item-controller> DELETE calls delete-label
- <label-estimate-controller> GET calls estimate-todo-count
- Controllers only handle HTTP concerns
- All label tests still pass

---

## Phase 6: Controller Cleanup

### Task 6.1: Remove duplicate get-authenticated-user from auth-controller
**Priority**: Low  
**Depends on**: Task 0.2, 1.7  
**Estimated effort**: 30 minutes  
**Description**: Replace local get-authenticated-user with helper version
**Completion criteria**:
- Import from auth-helper
- Remove local implementation
- All tests still pass

### Task 6.2: Remove duplicate get-authenticated-user from todos-controller
**Priority**: Low  
**Depends on**: Task 0.2, 2.9  
**Estimated effort**: 30 minutes  
**Description**: Replace local get-authenticated-user with helper version
**Completion criteria**:
- Import from auth-helper
- Remove local implementation
- All tests still pass

### Task 6.3: Remove duplicate get-authenticated-user from tags-controller
**Priority**: Low  
**Depends on**: Task 0.2, 3.8  
**Estimated effort**: 30 minutes  
**Description**: Replace local get-authenticated-user with helper version
**Completion criteria**:
- Import from auth-helper
- Remove local implementation
- All tests still pass

### Task 6.4: Remove duplicate get-authenticated-user from labels-controller
**Priority**: Low  
**Depends on**: Task 0.2, 5.9  
**Estimated effort**: 30 minutes  
**Description**: Replace local get-authenticated-user with helper version
**Completion criteria**:
- Import from auth-helper
- Remove local implementation
- All tests still pass

### Task 6.5: Remove duplicate get-authenticated-user from todo-tags-controller
**Priority**: Low  
**Depends on**: Task 0.2, 4.5  
**Estimated effort**: 30 minutes  
**Description**: Replace local get-authenticated-user with helper version
**Completion criteria**:
- Import from auth-helper
- Remove local implementation
- All tests still pass

### Task 6.6: Replace JSON conversion functions in controllers
**Priority**: Low  
**Depends on**: Task 0.3, 1.7, 2.9, 3.8, 4.5, 5.9  
**Estimated effort**: 2 hours  
**Description**: Replace local JSON conversion functions with helper versions
**Completion criteria**:
- All controllers import from json-converters
- Remove duplicate implementations
- All tests still pass

---

## Phase 7: Integration Testing and Documentation

### Task 7.1: Run full test suite
**Priority**: High  
**Depends on**: Task 6.6  
**Estimated effort**: 1 hour  
**Description**: Execute all tests to verify refactoring integrity
**Completion criteria**:
- All model tests pass
- All service tests pass
- All controller tests pass
- No regressions detected

### Task 7.2: Test transaction management
**Priority**: High  
**Depends on**: Task 7.1  
**Estimated effort**: 2 hours  
**Description**: Verify transaction boundaries work correctly
**Completion criteria**:
- Test multi-model operations rollback on error
- Verify label-with-tags transaction atomicity
- Verify todo-tags assignment transaction atomicity
- Document transaction patterns

### Task 7.3: Update application-loader.lisp
**Priority**: High  
**Depends on**: Task 7.1  
**Estimated effort**: 1 hour  
**Description**: Ensure all new services are loaded
**Completion criteria**:
- All service files imported
- All helper files imported
- Application loads without errors

### Task 7.4: Update CONTRIBUTING.md
**Priority**: Medium  
**Depends on**: Task 7.1  
**Estimated effort**: 2 hours  
**Description**: Document new architecture and patterns
**Completion criteria**:
- Explain 3-tier architecture
- Document when to use services vs models
- Provide service creation template
- Include examples from implemented services

### Task 7.5: Create architecture diagram
**Priority**: Low  
**Depends on**: Task 7.4  
**Estimated effort**: 2 hours  
**Description**: Create visual representation of new architecture
**Completion criteria**:
- Diagram shows Controller -> Service -> Model layers
- Show transaction boundaries
- Show helper utilities
- Add to docs/ directory

---

## Summary

**Total estimated effort**: ~90 hours

**Critical path**:
1. Phase 0 (Infrastructure): Tasks 0.1, 0.2, 0.3
2. Phase 1 (Auth): Tasks 1.1-1.7
3. Phase 2 (TODO): Tasks 2.1-2.9
4. Phase 3 (Tags): Tasks 3.1-3.8
5. Phase 4 (TODO-Tags): Tasks 4.1-4.5
6. Phase 5 (Labels): Tasks 5.1-5.9
7. Phase 6 (Cleanup): Tasks 6.1-6.6
8. Phase 7 (Testing): Tasks 7.1-7.5

**Parallelization opportunities**:
- Tasks 0.2 and 0.3 can be done in parallel
- After Phase 1 completes, Phases 2-5 can be partially parallelized
- Phase 6 cleanup tasks can be done in parallel after their dependencies

**Risk areas**:
- Transaction management in multi-model operations
- Breaking existing functionality during controller refactoring
- Session management edge cases in auth service
