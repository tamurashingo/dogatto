# Database Schema

This document describes the database schema for DOGATTO.

## Overview

DOGATTO uses MySQL 8.0 as the primary database. The schema is designed to support a tag-based TODO management system with the following core entities:

- Users
- TODOs
- Tags
- Labels
- TODO Comments

## Entity Relationship Diagram

```
┌─────────┐
│  Users  │
└────┬────┘
     │
     ├─────────┐
     │         │
┌────▼────┐ ┌──▼──────────┐
│  TODOs  │ │    Tags     │
└────┬────┘ └──┬──────────┘
     │         │
     ├─────────┤
     │    ┌────▼──────┐
     │    │ TODO-Tags │ (junction)
     │    └───────────┘
     │
┌────▼──────────┐
│ TODO-Comments │
└───────────────┘

┌────────┐
│ Labels │
└───┬────┘
    │
┌───▼──────┐
│Label-Tags│ (junction)
└──────────┘
```

## Tables

### users

Stores user account information.

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| id | VARCHAR(26) | PRIMARY KEY | ULID |
| username | VARCHAR(255) | NOT NULL, UNIQUE | Username |
| email | VARCHAR(255) | NOT NULL, UNIQUE | Email address |
| password-hash | VARCHAR(255) | NOT NULL | Hashed password |
| created-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP | Creation timestamp |
| updated-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP ON UPDATE | Last update timestamp |

**Indexes:**
- PRIMARY KEY: `id`
- UNIQUE: `username`
- UNIQUE: `email`
- INDEX: `created-at`

### todos

Stores TODO items.

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| id | VARCHAR(26) | PRIMARY KEY | ULID |
| user-id | VARCHAR(26) | NOT NULL, FOREIGN KEY | User who owns this TODO |
| title | VARCHAR(255) | NOT NULL | TODO title |
| description | TEXT | NULL | Detailed description |
| status | ENUM | NOT NULL, DEFAULT 'pending' | Status: pending, in-progress, completed, archived |
| due-date | TIMESTAMP | NULL | Due date |
| completed-at | TIMESTAMP | NULL | Completion timestamp |
| created-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP | Creation timestamp |
| updated-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP ON UPDATE | Last update timestamp |

**Indexes:**
- PRIMARY KEY: `id`
- FOREIGN KEY: `user-id` REFERENCES `users(id)` ON DELETE CASCADE
- INDEX: `user-id`
- INDEX: `status`
- INDEX: `due-date`
- INDEX: `completed-at`
- INDEX: `created-at`
- COMPOSITE: `(user-id, status)`
- COMPOSITE: `(user-id, due-date)`

### todo-comments

Stores comments on TODO items.

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| id | VARCHAR(26) | PRIMARY KEY | ULID |
| todo-id | VARCHAR(26) | NOT NULL, FOREIGN KEY | Associated TODO |
| user-id | VARCHAR(26) | NOT NULL, FOREIGN KEY | Comment author |
| comment | TEXT | NOT NULL | Comment text |
| created-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP | Creation timestamp |
| updated-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP ON UPDATE | Last update timestamp |

**Indexes:**
- PRIMARY KEY: `id`
- FOREIGN KEY: `todo-id` REFERENCES `todos(id)` ON DELETE CASCADE
- FOREIGN KEY: `user-id` REFERENCES `users(id)` ON DELETE CASCADE
- INDEX: `todo-id`
- INDEX: `created-at`
- COMPOSITE: `(todo-id, created-at)`

### tags

Stores tags that can be attached to TODOs.

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| id | VARCHAR(26) | PRIMARY KEY | ULID |
| user-id | VARCHAR(26) | NOT NULL, FOREIGN KEY | User who owns this tag |
| name | VARCHAR(100) | NOT NULL | Tag name |
| color | VARCHAR(7) | NULL | Hex color code (e.g., #FF5733) |
| created-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP | Creation timestamp |
| updated-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP ON UPDATE | Last update timestamp |

**Indexes:**
- PRIMARY KEY: `id`
- FOREIGN KEY: `user-id` REFERENCES `users(id)` ON DELETE CASCADE
- INDEX: `user-id`
- INDEX: `name`
- UNIQUE COMPOSITE: `(user-id, name)` - Tag names must be unique per user

### todo-tags

Junction table linking TODOs and Tags (many-to-many).

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| todo-id | VARCHAR(26) | FOREIGN KEY | TODO reference |
| tag-id | VARCHAR(26) | FOREIGN KEY | Tag reference |

**Indexes:**
- PRIMARY KEY: `(todo-id, tag-id)`
- FOREIGN KEY: `todo-id` REFERENCES `todos(id)` ON DELETE CASCADE
- FOREIGN KEY: `tag-id` REFERENCES `tags(id)` ON DELETE CASCADE
- INDEX: `todo-id`
- INDEX: `tag-id`

### labels

Stores labels that group multiple tags together.

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| id | VARCHAR(26) | PRIMARY KEY | ULID |
| user-id | VARCHAR(26) | NOT NULL, FOREIGN KEY | User who owns this label |
| name | VARCHAR(100) | NOT NULL | Label name |
| description | TEXT | NULL | Label description |
| created-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP | Creation timestamp |
| updated-at | TIMESTAMP | NOT NULL, DEFAULT CURRENT_TIMESTAMP ON UPDATE | Last update timestamp |

**Indexes:**
- PRIMARY KEY: `id`
- FOREIGN KEY: `user-id` REFERENCES `users(id)` ON DELETE CASCADE
- INDEX: `user-id`
- UNIQUE COMPOSITE: `(user-id, name)` - Label names must be unique per user

### label-tags

Junction table linking Labels and Tags (many-to-many).

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| label-id | VARCHAR(26) | FOREIGN KEY | Label reference |
| tag-id | VARCHAR(26) | FOREIGN KEY | Tag reference |

**Indexes:**
- PRIMARY KEY: `(label-id, tag-id)`
- FOREIGN KEY: `label-id` REFERENCES `labels(id)` ON DELETE CASCADE
- FOREIGN KEY: `tag-id` REFERENCES `tags(id)` ON DELETE CASCADE
- INDEX: `label-id`
- INDEX: `tag-id`

## Design Decisions

### ULID for Primary Keys

We use ULID (Universally Unique Lexicographically Sortable Identifier) instead of auto-incrementing integers:

**Advantages:**
- 26-character string representation
- Lexicographically sortable (can sort by ID to get chronological order)
- No coordination required between database instances
- Cannot guess other record IDs
- 128-bit UUID-compatible

**Format:** `01ARZ3NDEKTSV4RRFFQ69G5FAV`

### Kebab-case Column Names

All column names use kebab-case (e.g., `user-id`, `created-at`) instead of snake_case or camelCase:
- Consistent with Common Lisp naming conventions
- More readable in Lisp code
- Natural mapping to Lisp symbols

### Timestamps

All tables include:
- `created-at`: Set once on creation
- `updated-at`: Automatically updated on modification

These fields are managed by MySQL `DEFAULT CURRENT_TIMESTAMP` and `ON UPDATE CURRENT_TIMESTAMP`.

### Cascade Deletion

Foreign keys use `ON DELETE CASCADE` to maintain referential integrity:
- Deleting a user deletes all their TODOs, tags, labels, and comments
- Deleting a TODO deletes its comments and tag associations
- Deleting a tag removes it from all TODOs and labels

### Indexing Strategy

Indexes are added for:
1. **Foreign keys**: For join performance
2. **Frequently queried columns**: `status`, `due-date`, `completed-at`
3. **Composite indexes**: For common query patterns (e.g., user's pending TODOs)
4. **Unique constraints**: To prevent duplicates

## Migrations

Database schema is managed through migrations located in `db/migrate/`.

### Running Migrations

```bash
# Run all pending migrations
make db.migrate

# Rollback last migration
make db.rollback
```

### Creating Migrations

```bash
clails generate:migration <migration-name>
```

### Migration Naming Convention

Format: `YYYYMMDDHHMMSS_description.lisp`

Example: `20260111155249_create-users-table.lisp`

## Query Examples

### Get user's pending TODOs with tags

```sql
SELECT t.*, GROUP_CONCAT(tg.name) as tags
FROM todos t
LEFT JOIN todo_tags tt ON t.id = tt.todo_id
LEFT JOIN tags tg ON tt.tag_id = tg.id
WHERE t.user_id = ? AND t.status = 'pending'
GROUP BY t.id;
```

### Get TODOs due today

```sql
SELECT *
FROM todos
WHERE user_id = ?
  AND DATE(due_date) = CURDATE()
  AND status != 'completed';
```

### Get tag usage statistics

```sql
SELECT tg.name, COUNT(tt.todo_id) as todo_count
FROM tags tg
LEFT JOIN todo_tags tt ON tg.id = tt.tag_id
WHERE tg.user_id = ?
GROUP BY tg.id, tg.name
ORDER BY todo_count DESC;
```

## Backup and Restore

### Backup

```bash
docker compose exec mysql mysqldump -u dogatto -p dogatto_development > backup.sql
```

### Restore

```bash
docker compose exec -T mysql mysql -u dogatto -p dogatto_development < backup.sql
```

## Future Enhancements

Potential schema improvements:

1. **Full-text search**: Add full-text indexes on `title` and `description`
2. **Attachments**: Add table for file attachments to TODOs
3. **Recurring TODOs**: Add fields for recurrence patterns
4. **TODO templates**: Add table for reusable TODO templates
5. **Activity log**: Add table to track all changes
6. **Sharing**: Add tables for sharing TODOs between users
