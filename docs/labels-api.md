# Labels API Documentation

## Overview

The Labels API allows users to organize TODOs using labels. Each label consists of a name, description, and associated tags. Labels help users categorize and filter their TODOs based on multiple tags.

## Base URL

All API endpoints are prefixed with:
```
/api/v1
```

## Authentication

All label endpoints require authentication. Include the session cookie in your requests.

**Unauthenticated Response:**
```json
{
  "status": "error",
  "message": "Authentication required"
}
```

## Endpoints

### List All Labels

Get all labels for the authenticated user with optional filtering and searching.

**Endpoint:**
```
GET /api/v1/labels
```

**Query Parameters:**
- `page` (optional): Page number for pagination (default: 1)
- `per_page` (optional): Items per page (default: 20)
- `sort` (optional): Sort field (`name`, `created_at`, `updated_at`)
- `order` (optional): Sort order (`asc`, `desc`)
- `filter` (optional): Filter labels (`all`, `used`, `unused`)
- `search_mode` (optional): Search mode (`label_name`, `tag_name`)
- `q` (optional): Search query string

**Example Request:**
```
GET /api/v1/labels?search_mode=tag_name&q=Work
```

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "labels": [
      {
        "id": 1,
        "ulid": "01HQWE123456789ABCDEFGHJK",
        "name": "Development Tasks",
        "description": "Tasks related to software development",
        "tagCount": 3,
        "todoCount": 12,
        "createdAt": 1704067200,
        "updatedAt": 1704067200
      },
      {
        "id": 2,
        "ulid": "01HQWF987654321ZYXWVUTSRQP",
        "name": "Client Projects",
        "description": "Tasks for client deliverables",
        "tagCount": 2,
        "todoCount": 8,
        "createdAt": 1704070800,
        "updatedAt": 1704070800
      }
    ],
    "stats": {
      "totalLabels": 15,
      "usedLabels": 12,
      "unusedLabels": 3
    }
  }
}
```

**Statistics Fields:**
- `tagCount`: Number of tags associated with this label
- `todoCount`: Number of TODOs matching this label's tags (OR condition)
- `totalLabels`: Total number of labels for the user
- `usedLabels`: Number of labels with at least one TODO
- `unusedLabels`: Number of labels with no TODOs

---

### Create Label

Create a new label with associated tags.

**Endpoint:**
```
POST /api/v1/labels
```

**Request Body:**
```json
{
  "name": "Development Tasks",
  "description": "Tasks related to software development",
  "tagUlids": [
    "01HQWE123456789ABCDEFGHJK",
    "01HQWF987654321ZYXWVUTSRQP"
  ]
}
```

**Fields:**
- `name` (required): Label name (1-100 characters, unique per user)
- `description` (optional): Label description (max 500 characters)
- `tagUlids` (required): Array of tag ULIDs (at least one required)

**Response:** `201 Created`
```json
{
  "status": "success",
  "data": {
    "label": {
      "id": 1,
      "ulid": "01HQWE123456789ABCDEFGHJK",
      "name": "Development Tasks",
      "description": "Tasks related to software development",
      "tagCount": 2,
      "todoCount": 0,
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
  "message": "Label name is required"
}
```

`400 Bad Request` - Missing tags:
```json
{
  "status": "error",
  "message": "At least one tag is required"
}
```

`409 Conflict` - Duplicate label name:
```json
{
  "status": "error",
  "message": "Label with this name already exists"
}
```

---

### Get Label Details

Get a specific label by ULID, including associated tags.

**Endpoint:**
```
GET /api/v1/labels/:ulid
```

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "label": {
      "id": 1,
      "ulid": "01HQWE123456789ABCDEFGHJK",
      "name": "Development Tasks",
      "description": "Tasks related to software development",
      "tagCount": 2,
      "todoCount": 12,
      "createdAt": 1704067200,
      "updatedAt": 1704067200,
      "tags": [
        {
          "id": 1,
          "ulid": "01HQWE123456789ABCDEFGHJK",
          "name": "Work",
          "color": "#3B82F6"
        },
        {
          "id": 2,
          "ulid": "01HQWF987654321ZYXWVUTSRQP",
          "name": "Urgent",
          "color": "#EF4444"
        }
      ]
    }
  }
}
```

**Error Responses:**

`404 Not Found` - Label not found:
```json
{
  "status": "error",
  "message": "Label not found"
}
```

---

### Update Label

Update an existing label.

**Endpoint:**
```
PUT /api/v1/labels/:ulid
```

**Request Body:**
```json
{
  "name": "Updated Development Tasks",
  "description": "Updated description",
  "tagUlids": [
    "01HQWE123456789ABCDEFGHJK",
    "01HQWF987654321ZYXWVUTSRQP",
    "01HQWG456789012NMLKJIHGFED"
  ]
}
```

**Fields:**
- `name` (optional): Updated label name
- `description` (optional): Updated description
- `tagUlids` (optional): Updated array of tag ULIDs

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "label": {
      "id": 1,
      "ulid": "01HQWE123456789ABCDEFGHJK",
      "name": "Updated Development Tasks",
      "description": "Updated description",
      "tagCount": 3,
      "todoCount": 15,
      "createdAt": 1704067200,
      "updatedAt": 1704153600
    }
  }
}
```

**Error Responses:**

`404 Not Found` - Label not found:
```json
{
  "status": "error",
  "message": "Label not found"
}
```

`400 Bad Request` - Validation error:
```json
{
  "status": "error",
  "message": "Validation failed"
}
```

---

### Delete Label

Delete a label. This removes the label and its tag associations but does not delete the tags or TODOs.

**Endpoint:**
```
DELETE /api/v1/labels/:ulid
```

**Response:** `204 No Content`

No response body is returned on successful deletion.

**Error Responses:**

`404 Not Found` - Label not found:
```json
{
  "status": "error",
  "message": "Label not found"
}
```

---

### Estimate TODO Count

Estimate the number of TODOs that would match a given set of tags (OR condition).

**Endpoint:**
```
GET /api/v1/labels/estimate-todo-count
```

**Query Parameters:**
- `tag_ulids` (required): Comma-separated list of tag ULIDs

**Example Request:**
```
GET /api/v1/labels/estimate-todo-count?tag_ulids=01HQWE123,01HQWF987,01HQWG456
```

**Response:** `200 OK`
```json
{
  "status": "success",
  "data": {
    "count": 18
  }
}
```

**Error Responses:**

`400 Bad Request` - Missing parameter:
```json
{
  "status": "error",
  "message": "tag_ulids parameter is required"
}
```

---

## Search Modes

### Search by Label Name

Search labels by name using partial matching.

**Example:**
```
GET /api/v1/labels?search_mode=label_name&q=dev
```

Returns all labels where the name contains "dev" (case-insensitive).

### Search by Tag Name

Search labels by associated tag names.

**Example:**
```
GET /api/v1/labels?search_mode=tag_name&q=Work
```

Returns all labels that have tags with names containing "Work" (case-insensitive).

---

## Filter Options

- `all`: Show all labels (default)
- `used`: Show only labels with associated TODOs
- `unused`: Show only labels without associated TODOs

**Example:**
```
GET /api/v1/labels?filter=used
```

---

## TODO Matching Logic

When determining the TODO count for a label:
- Labels match TODOs based on their associated tags
- Multiple tags are combined with OR logic
- A TODO matches if it has **any** of the label's tags
- Example: Label with tags [Work, Urgent] matches TODOs that have Work OR Urgent (or both)

---

## Validation Rules

### Label Name
- Required for creation
- Must be 1-100 characters
- Must be unique per user (case-sensitive)
- Cannot contain only whitespace

### Description
- Optional
- Maximum 500 characters

### Tags
- At least one tag required for label creation
- Tag ULIDs must exist and belong to the user
- Duplicate tag ULIDs are ignored

---

## Common Error Responses

**Authentication Required:** `401 Unauthorized`
```json
{
  "status": "error",
  "message": "Authentication required"
}
```

**Resource Not Found:** `404 Not Found`
```json
{
  "status": "error",
  "message": "Label not found"
}
```

**Validation Error:** `400 Bad Request`
```json
{
  "status": "error",
  "message": "Validation failed"
}
```

**Server Error:** `500 Internal Server Error`
```json
{
  "status": "error",
  "message": "Internal server error"
}
```
