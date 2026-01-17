# Authentication API Documentation

## Overview

The authentication API provides endpoints for user registration, login, logout, and user profile management.

**Base URL**: `/api/v1/auth`

**Authentication**: Session-based authentication using HttpOnly cookies

---

## Endpoints

### Register User

Create a new user account.

**Endpoint**: `POST /api/v1/auth/register`

**Request Body**:
```json
{
  "name": "string",
  "email": "string",
  "password": "string"
}
```

**Validation**:
- `name`: Required, non-empty string
- `email`: Required, valid email format, must be unique
- `password`: Required, minimum 8 characters, must contain letters and numbers

**Response** (201 Created):
```json
{
  "status": "success",
  "data": {
    "user": {
      "id": 1,
      "name": "John Doe",
      "email": "john@example.com",
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "registrationStatus": "provisional",
      "createdAt": 1234567890,
      "updatedAt": 1234567890
    }
  }
}
```

**Error Responses**:

400 Bad Request - Missing or invalid fields:
```json
{
  "status": "error",
  "message": "Name is required"
}
```

400 Bad Request - Email already exists:
```json
{
  "status": "error",
  "message": "Email already registered"
}
```

400 Bad Request - Weak password:
```json
{
  "status": "error",
  "message": "Password validation failed",
  "errors": [
    "Password must be at least 8 characters long",
    "Password must contain at least one letter",
    "Password must contain at least one number"
  ]
}
```

---

### Login

Authenticate a user and create a session.

**Endpoint**: `POST /api/v1/auth/login`

**Request Body**:
```json
{
  "email": "string",
  "password": "string"
}
```

**Response** (200 OK):
```json
{
  "status": "success",
  "data": {
    "user": {
      "id": 1,
      "name": "John Doe",
      "email": "john@example.com",
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "registrationStatus": "provisional",
      "createdAt": 1234567890,
      "updatedAt": 1234567890
    }
  }
}
```

**Headers Set**:
```
Set-Cookie: session_id=<uuid>; Path=/; HttpOnly; SameSite=Strict; Max-Age=604800
```

**Error Responses**:

400 Bad Request - Missing fields:
```json
{
  "status": "error",
  "message": "Email is required"
}
```

401 Unauthorized - Invalid credentials:
```json
{
  "status": "error",
  "message": "Invalid email or password"
}
```

---

### Logout

Destroy the current session and clear the session cookie.

**Endpoint**: `POST /api/v1/auth/logout`

**Authentication**: Required (session cookie)

**Request Body**: None

**Response** (200 OK):
```json
{
  "status": "success",
  "message": "Logged out successfully"
}
```

**Headers Set**:
```
Set-Cookie: session_id=; Path=/; HttpOnly; SameSite=Strict; Max-Age=0
```

---

### Get Current User

Retrieve information about the currently authenticated user.

**Endpoint**: `GET /api/v1/auth/me`

**Authentication**: Required (session cookie)

**Response** (200 OK):
```json
{
  "status": "success",
  "data": {
    "user": {
      "id": 1,
      "name": "John Doe",
      "email": "john@example.com",
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "registrationStatus": "provisional",
      "createdAt": 1234567890,
      "updatedAt": 1234567890
    }
  }
}
```

**Error Responses**:

401 Unauthorized - Not authenticated:
```json
{
  "status": "error",
  "message": "Not authenticated"
}
```

401 Unauthorized - User not found:
```json
{
  "status": "error",
  "message": "User not found"
}
```

---

## Data Types

### User Object

```typescript
{
  id: number;              // Database ID
  name: string;            // User's display name
  email: string;           // User's email address
  ulid: string;            // ULID (26 characters)
  registrationStatus: string; // "provisional" or "active"
  createdAt: number;       // Unix timestamp
  updatedAt: number;       // Unix timestamp
}
```

**Note**: The `passwordHash` field is never included in API responses for security.

---

## Session Management

### Session Storage

- Sessions are stored in Redis with a 7-day TTL
- Session ID is a UUIDv4 string
- Session data includes:
  - `user-id`: User's database ID
  - `created-at`: Session creation timestamp
  - `expires-at`: Session expiration timestamp

### Session Cookie

- Name: `session_id`
- Attributes:
  - `HttpOnly`: Prevents JavaScript access
  - `SameSite=Strict`: CSRF protection
  - `Path=/`: Available on all paths
  - `Max-Age=604800`: 7 days

### Session Validation

The authentication middleware validates sessions on each request:
1. Extract `session_id` from cookies
2. Check if session exists in Redis
3. Verify session hasn't expired
4. Load user information
5. Add user to request context

---

## Security Considerations

### Password Security

- Passwords are hashed using PBKDF2 with 10,000 iterations
- Salt is randomly generated for each password
- Password hashes are never exposed in API responses

### Session Security

- Sessions expire after 7 days of inactivity
- Session IDs are UUIDs (cryptographically random)
- HttpOnly cookies prevent XSS attacks
- SameSite=Strict prevents CSRF attacks

### Input Validation

All inputs are validated:
- Email format validation
- Password strength requirements
- SQL injection protection (parameterized queries)
- XSS protection (JSON encoding)

---

## Rate Limiting

**Not Yet Implemented**

Future consideration:
- Login attempts: 5 per minute per IP
- Registration: 3 per hour per IP
- API calls: 100 per minute per user

---

## CORS Configuration

The API allows cross-origin requests from:
- Development: `http://localhost:3000`
- Production: Same-origin only

CORS headers:
- `Access-Control-Allow-Origin`: Configured origin
- `Access-Control-Allow-Methods`: GET, POST, PUT, DELETE, OPTIONS
- `Access-Control-Allow-Headers`: Content-Type, Authorization, X-CSRF-Token
- `Access-Control-Allow-Credentials`: true

---

## Error Response Format

All errors follow this format:

```json
{
  "status": "error",
  "message": "Human-readable error message",
  "errors": ["Optional array of detailed errors"]
}
```

### Common HTTP Status Codes

- `200 OK`: Request succeeded
- `201 Created`: Resource created successfully
- `400 Bad Request`: Invalid input or validation error
- `401 Unauthorized`: Authentication required or failed
- `409 Conflict`: Resource already exists (e.g., duplicate email)
- `500 Internal Server Error`: Server error

---

**Last Updated**: 2026-01-17  
**API Version**: 1.0
