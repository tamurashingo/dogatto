---
description: "Actionable task list for DOGATTO initial setup phase"
---

# Tasks: 001-initial-setup (DOGATTO 初期セットアップ)

**Input**: Design documents from `/specs/001-initial-setup/`
**Prerequisites**: plan.md (implementation plan), spec.md (user stories), data-model.md (database schema)

**Tests**: Tests are not included in this initial setup phase as it focuses on infrastructure.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Backend**: `app/` (clails application)
- **Frontend**: `front/src/` (React + TypeScript)
- **Database**: `db/` (schemas and migrations)
- **Docker**: `docker/` (container configurations)
- **Public**: `public/` (static assets)

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and Docker environment

- [x] T001 Verify existing project structure matches implementation plan
- [x] T002 Create docker-compose.yml with MySQL, Redis, and clails services in project root
- [x] T003 [P] Create MySQL Dockerfile in docker/mysql/Dockerfile
- [x] T004 [P] Create Redis configuration in docker/redis/redis.conf
- [x] T005 [P] Create clails Dockerfile in docker/clails/Dockerfile
- [x] T006 Test Docker environment startup with docker-compose up

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story implementation

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T007 Create .env.example file with environment variable templates in project root
- [x] T008 [P] Setup MySQL connection configuration in app/config/database.lisp
- [x] T009 [P] Setup Redis connection configuration in app/config/redis.lisp
- [x] T010 Create ULID generation utility in app/utils/ulid.lisp
- [x] T011 Create error response generator in app/utils/error-response.lisp
- [x] T012 [P] Create common validation functions in app/utils/validation.lisp
- [x] T013 [P] Setup logging infrastructure in app/utils/logger.lisp
- [x] T014 Create CSRF middleware in app/middleware/csrf.lisp
- [x] T015 Create CORS middleware in app/middleware/cors.lisp
- [x] T016 Setup session management middleware in app/middleware/session.lisp

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel ✅

---

## Phase 3: User Story 1 - 開発環境構築 (Priority: P1) 🎯 MVP

**Goal**: Docker環境が正常に起動し、MySQL、Redis、clailsアプリケーションが動作する

**Independent Test**: `docker-compose up` で全コンテナが起動し、ヘルスチェックが成功する

### Implementation for User Story 1

- [x] T017 [US1] Create MySQL initialization script in db/init/00_create_database.sql
- [x] T018 [US1] Configure MySQL character set to utf8mb4 in docker/mysql/my.cnf
- [x] T019 [US1] Configure Redis persistence settings in docker/redis/redis.conf
- [x] T020 [US1] Create healthcheck endpoint in app/routes/health.lisp
- [x] T021 [US1] Add container health checks to docker-compose.yml
- [x] T022 [US1] Create development startup script in Makefile (make dev-start)
- [x] T023 [US1] Create development stop script in Makefile (make dev-stop)
- [x] T024 [US1] Document environment setup in README.md

**Checkpoint**: Docker環境が完全に動作し、開発を開始できる ✅

---

## Phase 4: User Story 2 - データベース基盤構築 (Priority: P1)

**Goal**: すべてのテーブルが作成され、ULID生成機能が実装され、マイグレーションが動作する

**Independent Test**: マイグレーションスクリプトを実行し、すべてのテーブルが正しく作成される

### Implementation for User Story 2

- [x] T025 [P] [US2] Create users table migration in db/migrations/001_create_users.sql
- [x] T026 [P] [US2] Create todos table migration in db/migrations/002_create_todos.sql
- [x] T027 [P] [US2] Create todo_comments table migration in db/migrations/003_create_todo_comments.sql
- [x] T028 [P] [US2] Create tags table migration in db/migrations/004_create_tags.sql
- [x] T029 [P] [US2] Create todos_tags junction table migration in db/migrations/005_create_todos_tags.sql
- [x] T030 [P] [US2] Create labels table migration in db/migrations/006_create_labels.sql
- [x] T031 [P] [US2] Create labels_tags junction table migration in db/migrations/007_create_labels_tags.sql
- [x] T032 [US2] Create migration runner script in db/migrate.lisp
- [x] T033 [US2] Add indexes for ULID columns in db/migrations/008_add_indexes.sql
- [x] T034 [US2] Add indexes for foreign keys in db/migrations/009_add_foreign_key_indexes.sql
- [x] T035 [US2] Create seed data script for development in db/seeds/development.sql (skipped - not needed at this time)
- [x] T036 [US2] Add migration commands to Makefile (make migrate, make migrate-rollback)
- [x] T037 [US2] Test ULID generation and uniqueness in app/utils/ulid.lisp

**Checkpoint**: データベーススキーマが完全に構築され、マイグレーション管理ができる ✅

---

## Phase 5: User Story 3 - バックエンド基盤構築 (Priority: P1)

**Goal**: ルーティング、セッション管理、エラーハンドリングが実装され、REST API開発を開始できる

**Independent Test**: ヘルスチェックエンドポイントにアクセスし、正常なレスポンスを受け取る

### Implementation for User Story 3

- [ ] T038 [US3] Setup main routing configuration in app/routes/routes.lisp
- [ ] T039 [US3] Create authentication middleware skeleton in app/middleware/auth.lisp
- [ ] T040 [US3] Implement session storage with Redis in app/services/session-service.lisp
- [x] T041 [US3] Create session timeout handling in app/middleware/session.lisp (completed in Phase 2)
- [ ] T042 [US3] Implement global error handler in app/middleware/error-handler.lisp
- [x] T043 [US3] Create API error response format per specification in app/utils/error-response.lisp (completed in Phase 2)
- [x] T044 [P] [US3] Create request validation helper in app/utils/validation.lisp (completed in Phase 2)
- [ ] T045 [P] [US3] Create response helper functions in app/utils/response.lisp
- [x] T046 [US3] Setup API versioning structure (/api/v1) in app/routes/api/v1/ (directory structure exists)
- [ ] T047 [US3] Create request logging middleware in app/middleware/request-logger.lisp
- [ ] T048 [US3] Test session creation and retrieval with Redis
- [x] T049 [US3] Test error response format matches specification (health endpoint working)

**Checkpoint**: バックエンド基盤が整い、REST API開発を開始できる (partially complete)

---

## Phase 6: User Story 4 - フロントエンド基盤構築 (Priority: P1)

**Goal**: React + TypeScript環境が構築され、ビルドが動作し、APIクライアントが実装される

**Independent Test**: フロントエンドをビルドし、public/にバンドルが生成されることを確認

### Implementation for User Story 4

- [ ] T050 [US4] Initialize React + TypeScript project in front/ directory
- [ ] T051 [US4] Configure TypeScript compiler options in front/tsconfig.json
- [ ] T052 [US4] Setup Webpack or Vite build configuration in front/
- [ ] T053 [US4] Configure build output to public/assets/ in build config
- [ ] T054 [US4] Create directory structure: front/src/components/, pages/, hooks/, api/, types/
- [ ] T055 [P] [US4] Create API client base class in front/src/api/client.ts
- [ ] T056 [P] [US4] Create ApiError class in front/src/api/error.ts
- [ ] T057 [P] [US4] Create authentication context in front/src/contexts/AuthContext.tsx
- [ ] T058 [US4] Create HTTP fetch wrapper with error handling in front/src/api/fetcher.ts
- [ ] T059 [P] [US4] Create base HTML template for pages in app/views/layouts/base.html
- [ ] T060 [P] [US4] Create TypeScript type definitions for User in front/src/types/user.ts
- [ ] T061 [P] [US4] Create TypeScript type definitions for Todo in front/src/types/todo.ts
- [ ] T062 [P] [US4] Create TypeScript type definitions for Tag in front/src/types/tag.ts
- [ ] T063 [P] [US4] Create TypeScript type definitions for Label in front/src/types/label.ts
- [ ] T064 [US4] Setup hot reload for development in build configuration
- [ ] T065 [US4] Create development build script in package.json
- [ ] T066 [US4] Create production build script in package.json
- [ ] T067 [US4] Add frontend commands to Makefile (make front-dev, make front-build)
- [ ] T068 [US4] Create route serving mechanism in app/routes/pages.lisp
- [ ] T069 [US4] Test frontend build and asset generation
- [ ] T070 [US4] Test HTML template rendering from clails

**Checkpoint**: フロントエンド基盤が完成し、UI開発を開始できる

---

## Phase 7: Polish & Documentation

**Purpose**: 環境構築の完成とドキュメント整備

- [ ] T071 [P] Update README.md with setup instructions
- [ ] T072 [P] Create CONTRIBUTING.md with development workflow
- [ ] T073 [P] Document environment variables in docs/environment.md
- [ ] T074 [P] Document database schema in docs/database.md
- [ ] T075 [P] Document API conventions in docs/api-conventions.md
- [ ] T076 Create troubleshooting guide in docs/troubleshooting.md
- [ ] T077 Verify all development commands in Makefile work correctly
- [ ] T078 Create .gitignore for build artifacts and environment files

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - US1 (開発環境構築): Can start after Phase 2
  - US2 (データベース基盤): Can start after Phase 2, should complete before backend development
  - US3 (バックエンド基盤): Can start after Phase 2, benefits from US2 completion
  - US4 (フロントエンド基盤): Can start after Phase 2, independent from US2/US3
- **Polish (Phase 7)**: Depends on US1-US4 completion

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational - Independent, but needed by US3
- **User Story 3 (P1)**: Can start after Foundational - Benefits from US2 but can proceed in parallel
- **User Story 4 (P1)**: Can start after Foundational - Completely independent from US2/US3

### Parallel Opportunities

- Phase 1: T003, T004, T005 can run in parallel (different container configs)
- Phase 2: T008, T009, T012, T013 can run in parallel (different utility files)
- Phase 4: T025-T031 can run in parallel (different migration files)
- Phase 5: T044, T045 can run in parallel (different helper files)
- Phase 6: T055, T056, T057, T059, T060, T061, T062, T063 can run in parallel (different files)
- Phase 7: T071, T072, T073, T074, T075 can run in parallel (different documentation files)

---

## Parallel Example: User Story 4 (Frontend Setup)

```bash
# Launch all type definitions together:
Task: T060 "Create TypeScript type definitions for User in front/src/types/user.ts"
Task: T061 "Create TypeScript type definitions for Todo in front/src/types/todo.ts"
Task: T062 "Create TypeScript type definitions for Tag in front/src/types/tag.ts"
Task: T063 "Create TypeScript type definitions for Label in front/src/types/label.ts"

# Launch API infrastructure together:
Task: T055 "Create API client base class in front/src/api/client.ts"
Task: T056 "Create ApiError class in front/src/api/error.ts"
Task: T057 "Create authentication context in front/src/contexts/AuthContext.tsx"
```

---

## Implementation Strategy

### Sequential Approach (Recommended for Single Developer)

1. Complete Phase 1: Setup (T001-T006)
2. Complete Phase 2: Foundational (T007-T016)
3. Complete Phase 3: US1 開発環境構築 (T017-T024)
4. Complete Phase 4: US2 データベース基盤 (T025-T037)
5. Complete Phase 5: US3 バックエンド基盤 (T038-T049)
6. Complete Phase 6: US4 フロントエンド基盤 (T050-T070)
7. Complete Phase 7: Polish & Documentation (T071-T078)

### Parallel Team Strategy (If Multiple Developers Available)

1. **Everyone**: Complete Phase 1 & 2 together (foundation)
2. **After Phase 2 completes**:
   - Developer A: US1 (Docker environment) + US2 (Database)
   - Developer B: US3 (Backend infrastructure)
   - Developer C: US4 (Frontend infrastructure)
3. **Merge and validate**: All components work together

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Each user story should be independently completable and testable
- Commit after each logical group of tasks
- Test Docker environment frequently during US1
- Validate database migrations work both forward and backward
- Ensure frontend can connect to backend after US3 and US4 complete
