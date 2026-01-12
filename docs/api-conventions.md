# API Conventions

This document describes the API conventions and standards used in DOGATTO.

## Base URL

```
http://localhost:5000/api/v1
```

## Authentication

**Status:** Not yet implemented (planned for Phase 1 features)

When implemented, the API will use session-based authentication:
- Login returns a session token
- Token must be included in subsequent requests
- Token stored in HTTP-only cookie or Authorization header

## Request Format

### HTTP Methods

| Method | Usage |
|--------|-------|
| GET | Retrieve resources |
| POST | Create new resources |
| PUT | Replace entire resource |
| PATCH | Partially update resource |
| DELETE | Delete resource |

### Content Type

All requests with body must use:
```
Content-Type: application/json
```

### Request Body

Request bodies should be valid JSON:

```json
{
  "title": "Buy groceries",
  "description": "Milk, eggs, bread",
  "status": "pending",
  "dueDate": "2026-01-15T10:00:00Z"
}
```

## Response Format

### Success Response

Status: `200 OK`, `201 Created`, `204 No Content`

```json
{
  "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "title": "Buy groceries",
  "description": "Milk, eggs, bread",
  "status": "pending",
  "dueDate": "2026-01-15T10:00:00Z",
  "createdAt": "2026-01-12T10:00:00Z",
  "updatedAt": "2026-01-12T10:00:00Z"
}
```

### Error Response

Status: `4xx` or `5xx`

```json
{
  "error": {
    "message": "Validation failed",
    "code": "VALIDATION_ERROR",
    "details": {
      "title": ["Title is required"],
      "dueDate": ["Must be a valid date"]
    }
  }
}
```

## Status Codes

| Code | Meaning | Usage |
|------|---------|-------|
| 200 | OK | Successful GET, PUT, PATCH, DELETE |
| 201 | Created | Successful POST |
| 204 | No Content | Successful DELETE with no response body |
| 400 | Bad Request | Invalid request format |
| 401 | Unauthorized | Authentication required |
| 403 | Forbidden | Authenticated but not authorized |
| 404 | Not Found | Resource doesn't exist |
| 422 | Unprocessable Entity | Validation error |
| 500 | Internal Server Error | Server error |

## Naming Conventions

### Field Names

Use camelCase for JSON field names:

```json
{
  "id": "...",
  "userId": "...",
  "createdAt": "...",
  "updatedAt": "...",
  "dueDate": "..."
}
```

**Rationale:** JavaScript/TypeScript convention

### Endpoint Paths

Use kebab-case for multi-word resources:

```
/api/v1/todo-comments
/api/v1/label-tags
```

Use plural nouns for collections:

```
/api/v1/todos
/api/v1/tags
/api/v1/users
```

## Resource Endpoints

### Standard CRUD Pattern

For a resource (e.g., TODOs):

| Method | Path | Description |
|--------|------|-------------|
| GET | `/todos` | List all TODOs |
| GET | `/todos/:id` | Get specific TODO |
| POST | `/todos` | Create new TODO |
| PUT | `/todos/:id` | Replace TODO |
| PATCH | `/todos/:id` | Update TODO |
| DELETE | `/todos/:id` | Delete TODO |

### Nested Resources

For nested resources (e.g., TODO comments):

```
GET    /todos/:todoId/comments
POST   /todos/:todoId/comments
GET    /todos/:todoId/comments/:id
PATCH  /todos/:todoId/comments/:id
DELETE /todos/:todoId/comments/:id
```

## Pagination

List endpoints support pagination with query parameters:

```
GET /api/v1/todos?page=1&perPage=20
```

**Query Parameters:**
- `page`: Page number (1-indexed)
- `perPage`: Items per page (default: 20, max: 100)

**Response:**

```json
{
  "data": [...],
  "pagination": {
    "page": 1,
    "perPage": 20,
    "totalItems": 150,
    "totalPages": 8,
    "hasNext": true,
    "hasPrev": false
  }
}
```

## Filtering

Use query parameters for filtering:

```
GET /api/v1/todos?status=pending
GET /api/v1/todos?status=pending&dueDate[lte]=2026-01-20
GET /api/v1/todos?tags=work,urgent
```

**Filter Operators:**

- `field=value`: Exact match
- `field[gt]=value`: Greater than
- `field[gte]=value`: Greater than or equal
- `field[lt]=value`: Less than
- `field[lte]=value`: Less than or equal
- `field=value1,value2`: IN (multiple values)

## Sorting

Use `sort` query parameter:

```
GET /api/v1/todos?sort=dueDate
GET /api/v1/todos?sort=-createdAt  # Descending
GET /api/v1/todos?sort=status,-dueDate  # Multiple fields
```

**Convention:**
- Ascending: `sort=field`
- Descending: `sort=-field`

## Field Selection

Use `fields` query parameter to select specific fields:

```
GET /api/v1/todos?fields=id,title,status
```

**Response:**

```json
{
  "data": [
    {
      "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "title": "Buy groceries",
      "status": "pending"
    }
  ]
}
```

## Including Related Resources

Use `include` query parameter:

```
GET /api/v1/todos?include=tags,comments
```

**Response:**

```json
{
  "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "title": "Buy groceries",
  "tags": [
    { "id": "...", "name": "shopping" }
  ],
  "comments": [
    { "id": "...", "comment": "Don't forget butter" }
  ]
}
```

## Date and Time

Use ISO 8601 format with UTC timezone:

```json
{
  "createdAt": "2026-01-12T10:00:00Z",
  "dueDate": "2026-01-15T10:00:00Z"
}
```

## Validation Errors

Return detailed validation errors with field-level information:

```json
{
  "error": {
    "message": "Validation failed",
    "code": "VALIDATION_ERROR",
    "details": {
      "title": [
        "Title is required",
        "Title must be less than 255 characters"
      ],
      "dueDate": [
        "Must be a valid ISO 8601 date"
      ],
      "status": [
        "Must be one of: pending, in-progress, completed, archived"
      ]
    }
  }
}
```

## Idempotency

- **GET, PUT, DELETE**: Naturally idempotent
- **POST**: Not idempotent by default
- **PATCH**: Should be idempotent

For critical POST operations, consider adding an `Idempotency-Key` header.

## Rate Limiting

**Status:** Not yet implemented (planned for production)

When implemented:
- Include rate limit headers
- Return `429 Too Many Requests` when exceeded

```
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 999
X-RateLimit-Reset: 1640995200
```

## CORS

**Status:** Configured to allow same-origin requests

Cross-origin requests are not yet fully configured.

## Health Check

```
GET /health
```

**Response:**

```json
{
  "status": "healthy",
  "timestamp": 3977214804
}
```

## Versioning

API version is included in the URL path:

```
/api/v1/todos
```

Future versions will be:
```
/api/v2/todos
```

Old versions will be maintained for backward compatibility.

## Examples

### Create TODO

**Request:**
```http
POST /api/v1/todos
Content-Type: application/json

{
  "title": "Write documentation",
  "description": "Complete API docs",
  "status": "pending",
  "dueDate": "2026-01-20T17:00:00Z",
  "tagIds": ["01ARZ3...", "01ARZ4..."]
}
```

**Response:**
```http
HTTP/1.1 201 Created
Content-Type: application/json

{
  "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "userId": "01ARZ3NDEKTSV4RRFFQ69G5FAA",
  "title": "Write documentation",
  "description": "Complete API docs",
  "status": "pending",
  "dueDate": "2026-01-20T17:00:00Z",
  "completedAt": null,
  "createdAt": "2026-01-12T10:00:00Z",
  "updatedAt": "2026-01-12T10:00:00Z"
}
```

### Update TODO Status

**Request:**
```http
PATCH /api/v1/todos/01ARZ3NDEKTSV4RRFFQ69G5FAV
Content-Type: application/json

{
  "status": "completed"
}
```

**Response:**
```http
HTTP/1.1 200 OK
Content-Type: application/json

{
  "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "status": "completed",
  "completedAt": "2026-01-12T11:30:00Z",
  "updatedAt": "2026-01-12T11:30:00Z"
}
```

### List TODOs with Filters

**Request:**
```http
GET /api/v1/todos?status=pending&sort=-dueDate&include=tags&page=1&perPage=10
```

**Response:**
```http
HTTP/1.1 200 OK
Content-Type: application/json

{
  "data": [
    {
      "id": "01ARZ3...",
      "title": "Write documentation",
      "status": "pending",
      "dueDate": "2026-01-20T17:00:00Z",
      "tags": [
        { "id": "01ARZ4...", "name": "work" }
      ]
    }
  ],
  "pagination": {
    "page": 1,
    "perPage": 10,
    "totalItems": 42,
    "totalPages": 5,
    "hasNext": true,
    "hasPrev": false
  }
}
```

### Error Example

**Request:**
```http
POST /api/v1/todos
Content-Type: application/json

{
  "title": "",
  "status": "invalid"
}
```

**Response:**
```http
HTTP/1.1 422 Unprocessable Entity
Content-Type: application/json

{
  "error": {
    "message": "Validation failed",
    "code": "VALIDATION_ERROR",
    "details": {
      "title": ["Title is required"],
      "status": ["Must be one of: pending, in-progress, completed, archived"]
    }
  }
}
```

## Client Libraries

### TypeScript/JavaScript

Use the provided API client:

```typescript
import { apiClient } from '@/api/client';

// GET request
const todos = await apiClient.get<Todo[]>('/todos');

// POST request
const newTodo = await apiClient.post<Todo>('/todos', {
  title: 'New TODO',
  status: 'pending'
});

// Error handling
try {
  await apiClient.post('/todos', data);
} catch (error) {
  if (error instanceof ApiError) {
    console.log(error.status, error.code, error.details);
  }
}
```

## Future Considerations

- GraphQL endpoint for complex queries
- WebSocket support for real-time updates
- Batch operations endpoint
- Export/import endpoints
- Webhook notifications
