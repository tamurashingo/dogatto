# Tags API Documentation

## Overview

The Tags API allows users to create, manage, and organize their TODOs using custom tags. Each tag has a name and color, and can be assigned to multiple TODOs.

## Base URL

All API endpoints are prefixed with:
```
/api/v1
```

## Authentication

All tag endpoints require authentication. Include the session cookie in your requests.

**Unauthenticated Response:**
```json
{
  "status": "error",
  "message": "Authentication required"
}
```

## Endpoints

### List All Tags

Get all tags for the authenticated user.

**Endpoint:**
```
GET /api/v1/tags
```

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "tags": [
      {
        "id": 1,
        "ulid": "01HQWE123456789ABCDEFGHJK",
        "name": "Work",
        "color": "#3B82F6",
        "todoCount": 5,
        "completedCount": 2,
        "activeCount": 3,
        "createdAt": 1704067200,
        "updatedAt": 1704067200
      },
      {
        "id": 2,
        "ulid": "01HQWF987654321ZYXWVUTSRQP",
        "name": "Personal",
        "color": "#10B981",
        "todoCount": 8,
        "completedCount": 5,
        "activeCount": 3,
        "createdAt": 1704070800,
        "updatedAt": 1704070800
      }
    ]
  }
}
```

**Statistics Fields:**
- `todoCount`: Total number of TODOs with this tag
- `completedCount`: Number of completed TODOs with this tag
- `activeCount`: Number of active (pending) TODOs with this tag

---

### Create Tag

Create a new tag.

**Endpoint:**
```
POST /api/v1/tags
```

**Request Body:**
```json
{
  "name": "Work",
  "color": "#3B82F6"
}
```

**Fields:**
- `name` (required): Tag name (1-50 characters, unique per user)
- `color` (optional): Hex color code (default: `#3B82F6`)

**Response:** `201 Created`
```json
{
  "status": "success",
  "data": {
    "tag": {
      "id": 1,
      "ulid": "01HQWE123456789ABCDEFGHJK",
      "name": "Work",
      "color": "#3B82F6",
      "todoCount": 0,
      "completedCount": 0,
      "activeCount": 0,
      "createdAt": 1704067200,
      "updatedAt": 1704067200
    }
  }
}
```

**Error Responses:**

`400 Bad Request` - Validation error:
```json
{
  "status": "error",
  "message": "Tag name is required"
}
```

`409 Conflict` - Duplicate tag name:
```json
{
  "status": "error",
  "message": "Tag with this name already exists"
}
```

---

### Get Tag Details

Get a specific tag by ULID.

**Endpoint:**
```
GET /api/v1/tags/:ulid
```

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "tag": {
      "id": 1,
      "ulid": "01HQWE123456789ABCDEFGHJK",
      "name": "Work",
      "color": "#3B82F6",
      "todoCount": 5,
      "completedCount": 2,
      "activeCount": 3,
      "createdAt": 1704067200,
      "updatedAt": 1704067200
    },
    "todos": [
      {
        "id": 10,
        "ulid": "01HQTODO123456789ABCDEFG",
        "title": "Complete project proposal",
        "content": "Draft and submit the Q1 project proposal",
        "status": "pending",
        "dueDate": 1704153600,
        "createdAt": 1704067200,
        "updatedAt": 1704067200,
        "completedAt": null,
        "tags": [
          {
            "ulid": "01HQWE123456789ABCDEFGHJK",
            "name": "Work",
            "color": "#3B82F6"
          }
        ]
      }
    ]
  }
}
```

**Error Responses:**

`404 Not Found`:
```json
{
  "status": "error",
  "message": "Tag not found"
}
```

`403 Forbidden` - Accessing another user's tag:
```json
{
  "status": "error",
  "message": "Access denied"
}
```

---

### Update Tag

Update an existing tag.

**Endpoint:**
```
PUT /api/v1/tags/:ulid
```

**Request Body:**
```json
{
  "name": "Work Projects",
  "color": "#2563EB"
}
```

**Fields:**
- `name` (optional): New tag name
- `color` (optional): New color code

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "tag": {
      "id": 1,
      "ulid": "01HQWE123456789ABCDEFGHJK",
      "name": "Work Projects",
      "color": "#2563EB",
      "todoCount": 5,
      "completedCount": 2,
      "activeCount": 3,
      "createdAt": 1704067200,
      "updatedAt": 1704150000
    }
  }
}
```

**Error Responses:**

`400 Bad Request`:
```json
{
  "status": "error",
  "message": "Invalid color code format"
}
```

`404 Not Found`:
```json
{
  "status": "error",
  "message": "Tag not found"
}
```

`409 Conflict` - Duplicate name:
```json
{
  "status": "error",
  "message": "Tag with this name already exists"
}
```

---

### Delete Tag

Delete a tag. This also removes the tag from all associated TODOs.

**Endpoint:**
```
DELETE /api/v1/tags/:ulid
```

**Response:** `204 No Content`

**Behavior:**
- Tag is permanently deleted
- All associations with TODOs are removed (CASCADE)
- TODOs themselves are not deleted

**Error Responses:**

`404 Not Found`:
```json
{
  "status": "error",
  "message": "Tag not found"
}
```

`403 Forbidden`:
```json
{
  "status": "error",
  "message": "Access denied"
}
```

---

## TODO-Tag Associations

### Assign Tags to TODO

Assign tags to a TODO. This replaces all existing tags with the new set.

**Endpoint:**
```
PUT /api/v1/todos/:ulid/tags
```

**Request Body:**
```json
{
  "tagUlids": [
    "01HQWE123456789ABCDEFGHJK",
    "01HQWF987654321ZYXWVUTSRQP"
  ]
}
```

**Fields:**
- `tagUlids` (required): Array of tag ULIDs (max 10 tags per TODO)

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "todo": {
      "id": 10,
      "ulid": "01HQTODO123456789ABCDEFG",
      "title": "Complete project proposal",
      "content": "Draft and submit the Q1 project proposal",
      "status": "pending",
      "dueDate": 1704153600,
      "tags": [
        {
          "ulid": "01HQWE123456789ABCDEFGHJK",
          "name": "Work",
          "color": "#3B82F6"
        },
        {
          "ulid": "01HQWF987654321ZYXWVUTSRQP",
          "name": "Important",
          "color": "#EF4444"
        }
      ],
      "createdAt": 1704067200,
      "updatedAt": 1704150000,
      "completedAt": null
    }
  }
}
```

**To remove all tags:**
```json
{
  "tagUlids": []
}
```

**Error Responses:**

`400 Bad Request` - Too many tags:
```json
{
  "status": "error",
  "message": "Maximum 10 tags allowed per TODO"
}
```

`400 Bad Request` - Invalid tag ULID:
```json
{
  "status": "error",
  "message": "Invalid tag ULID"
}
```

`404 Not Found` - TODO or tag not found:
```json
{
  "status": "error",
  "message": "TODO not found"
}
```

`403 Forbidden` - Trying to use another user's tag:
```json
{
  "status": "error",
  "message": "Access denied"
}
```

---

### Remove Tag from TODO

Remove a specific tag from a TODO.

**Endpoint:**
```
DELETE /api/v1/todos/:todoUlid/tags/:tagUlid
```

**Response:** `204 No Content`

**Behavior:**
- Removes the association between the TODO and tag
- The tag itself is not deleted
- The TODO is not deleted

**Error Responses:**

`404 Not Found`:
```json
{
  "status": "error",
  "message": "TODO or tag not found"
}
```

---

## TODO Filtering by Tags

### Filter TODOs by Tags

Filter TODOs by one or more tags (OR condition).

**Endpoint:**
```
GET /api/v1/todos?tags=:ulid1,:ulid2
```

**Query Parameters:**
- `tags`: Comma-separated list of tag ULIDs

**Example:**
```
GET /api/v1/todos?tags=01HQWE123456789ABCDEFGHJK,01HQWF987654321ZYXWVUTSRQP
```

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "todos": [
      {
        "id": 10,
        "ulid": "01HQTODO123456789ABCDEFG",
        "title": "Complete project proposal",
        "tags": [
          {
            "ulid": "01HQWE123456789ABCDEFGHJK",
            "name": "Work",
            "color": "#3B82F6"
          }
        ],
        ...
      }
    ]
  }
}
```

**Behavior:**
- Returns TODOs that have **any** of the specified tags (OR condition)
- Tags are included in the TODO response

---

### Filter Untagged TODOs

Get TODOs that have no tags assigned.

**Endpoint:**
```
GET /api/v1/todos?untagged=true
```

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "todos": [
      {
        "id": 15,
        "ulid": "01HQTODO987654321ZYXWVUT",
        "title": "Untagged task",
        "tags": [],
        ...
      }
    ]
  }
}
```

---

### Combined Filtering

Combine tag filtering with status filtering.

**Example:**
```
GET /api/v1/todos?tags=01HQWE123456789ABCDEFGHJK&status=active
```

Returns active TODOs that have the specified tag.

---

## Validation Rules

### Tag Name
- **Required**: Yes
- **Length**: 1-50 characters
- **Uniqueness**: Must be unique per user (case-insensitive)
- **Trimming**: Leading and trailing whitespace removed

### Tag Color
- **Required**: No (default: `#3B82F6`)
- **Format**: Hex color code (`#RRGGBB`)
- **Length**: Exactly 7 characters (including `#`)
- **Validation**: Must be valid hex format

### Tag Limit per TODO
- **Maximum**: 10 tags per TODO
- **Minimum**: 0 tags (untagged TODOs are allowed)

---

## Default Color Palette

The following colors are recommended for tags:

| Color Name | Hex Code  |
|------------|-----------|
| Blue       | #3B82F6   |
| Green      | #10B981   |
| Red        | #EF4444   |
| Yellow     | #F59E0B   |
| Purple     | #8B5CF6   |
| Pink       | #EC4899   |
| Gray       | #6B7280   |
| Cyan       | #06B6D4   |

---

## Error Handling

All errors follow the standard API error format:

```json
{
  "status": "error",
  "message": "Error description"
}
```

**Common HTTP Status Codes:**
- `200 OK`: Successful GET/PUT request
- `201 Created`: Successful POST request
- `204 No Content`: Successful DELETE request
- `400 Bad Request`: Validation error
- `401 Unauthorized`: Authentication required
- `403 Forbidden`: Authorization failed
- `404 Not Found`: Resource not found
- `409 Conflict`: Duplicate resource

---

## Best Practices

1. **Tag Organization**:
   - Use meaningful tag names
   - Limit tags to essential categories
   - Use colors consistently

2. **Performance**:
   - Fetch all tags once and cache in frontend
   - Use tag ULIDs for filtering (not names)
   - Batch tag assignments when possible

3. **User Experience**:
   - Show tag statistics to help users understand usage
   - Provide tag search/filter in UI
   - Limit displayed tags on TODO cards to avoid clutter

4. **Data Integrity**:
   - Validate tag ULIDs before sending to API
   - Handle cascade delete gracefully
   - Provide confirmation for tag deletion

---

## Examples

### Create a Tag and Assign to TODO

1. Create a tag:
```bash
curl -X POST http://localhost:5000/api/v1/tags \
  -H "Content-Type: application/json" \
  -d '{"name": "Urgent", "color": "#EF4444"}' \
  --cookie "session=your-session-id"
```

2. Assign to TODO:
```bash
curl -X PUT http://localhost:5000/api/v1/todos/01HQTODO123/tags \
  -H "Content-Type: application/json" \
  -d '{"tagUlids": ["01HQWE123456789ABCDEFGHJK"]}' \
  --cookie "session=your-session-id"
```

### Filter TODOs by Multiple Tags

```bash
curl http://localhost:5000/api/v1/todos?tags=01HQWE123,01HQWF987 \
  --cookie "session=your-session-id"
```

### Update Tag Color

```bash
curl -X PUT http://localhost:5000/api/v1/tags/01HQWE123456789ABCDEFGHJK \
  -H "Content-Type: application/json" \
  -d '{"color": "#10B981"}' \
  --cookie "session=your-session-id"
```

---

## See Also

- [API Conventions](api-conventions.md) - General API design patterns
- [Authentication API](auth-api.md) - Authentication endpoints
- [Database Schema](database.md) - Tags table structure
