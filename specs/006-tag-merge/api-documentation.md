# Tag Merge API Documentation

## Overview

Tag Merge APIは、複数のタグを1つのタグに統合する機能を提供します。

**Base URL**: `/api/v1`

**Authentication**: すべてのエンドポイントはセッションベースの認証が必要です。

---

## Endpoints

### 1. Merge Tags to Existing Tag

既存のタグに複数のソースタグをマージします。

#### Request

```http
POST /api/v1/tags/merge
Content-Type: application/json
Cookie: session_id=<session_id>
```

**Body Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| source_ulids | string[] | Yes | マージ元タグのULID配列 |
| target_ulid | string | Yes | マージ先タグのULID |

**Example Request**:

```json
{
  "source_ulids": [
    "01KHFFS79PW4SBJK3MGAQ8EW9C",
    "01KHFFV3VSRM9YPC4AVSK6K7YY"
  ],
  "target_ulid": "01KHFG2HP5E8N9QXJC5W7VKTM3"
}
```

#### Response

**Success (200 OK)**:

```json
{
  "status": "success",
  "data": {
    "mergedTags": [
      {
        "ulid": "01KHFFS79PW4SBJK3MGAQ8EW9C",
        "name": "JavaScript",
        "color": "#3B82F6",
        "mergedToUlid": "01KHFG2HP5E8N9QXJC5W7VKTM3",
        "mergedAt": "2026-02-16T12:00:00Z",
        "createdAt": "2026-01-10T10:00:00Z",
        "updatedAt": "2026-02-16T12:00:00Z"
      },
      {
        "ulid": "01KHFFV3VSRM9YPC4AVSK6K7YY",
        "name": "JS",
        "color": "#3B82F6",
        "mergedToUlid": "01KHFG2HP5E8N9QXJC5W7VKTM3",
        "mergedAt": "2026-02-16T12:00:00Z",
        "createdAt": "2026-01-12T15:00:00Z",
        "updatedAt": "2026-02-16T12:00:00Z"
      }
    ],
    "targetTag": {
      "ulid": "01KHFG2HP5E8N9QXJC5W7VKTM3",
      "name": "Frontend",
      "color": "#10B981",
      "todoCount": 25,
      "completedCount": 10,
      "activeCount": 15,
      "createdAt": "2026-01-05T10:00:00Z",
      "updatedAt": "2026-02-16T12:00:00Z"
    }
  }
}
```

**Response Fields**:

- `mergedTags`: マージされたタグの配列
  - `ulid`: タグULID
  - `name`: タグ名
  - `color`: タグカラー（16進数カラーコード）
  - `mergedToUlid`: マージ先タグのULID
  - `mergedAt`: マージ日時（ISO 8601形式）
  - `createdAt`: 作成日時
  - `updatedAt`: 更新日時

- `targetTag`: マージ先タグの情報
  - `ulid`: タグULID
  - `name`: タグ名
  - `color`: タグカラー
  - `todoCount`: 関連するTODO数（合計）
  - `completedCount`: 完了TODO数
  - `activeCount`: アクティブTODO数
  - `createdAt`: 作成日時
  - `updatedAt`: 更新日時

**Error Responses**:

**400 Bad Request** - source_ulids未指定:
```json
{
  "status": "error",
  "message": "source_ulids is required"
}
```

**400 Bad Request** - target_ulid未指定:
```json
{
  "status": "error",
  "message": "target_ulid is required"
}
```

**400 Bad Request** - バリデーションエラー:
```json
{
  "status": "error",
  "message": "Validation failed",
  "errors": [
    "Source tag 'JavaScript' does not exist",
    "Target tag cannot be one of the source tags"
  ]
}
```

**401 Unauthorized** - 認証エラー:
```json
{
  "status": "error",
  "message": "Authentication required"
}
```

**403 Forbidden** - 権限エラー:
```json
{
  "status": "error",
  "message": "Access denied: You do not own this tag"
}
```

**404 Not Found** - タグが見つからない:
```json
{
  "status": "error",
  "message": "Tag not found or not accessible"
}
```

**500 Internal Server Error** - サーバーエラー:
```json
{
  "status": "error",
  "message": "Internal server error: <error details>"
}
```

---

### 2. Merge Tags to New Tag

新しいタグを作成して、複数のソースタグをマージします。

#### Request

```http
POST /api/v1/tags/merge-to-new
Content-Type: application/json
Cookie: session_id=<session_id>
```

**Body Parameters**:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| source_ulids | string[] | Yes | マージ元タグのULID配列 |
| new_tag | object | Yes | 新しいタグの情報 |
| new_tag.name | string | Yes | タグ名（1-50文字） |
| new_tag.color | string | No | カラーコード（デフォルト: #3B82F6） |

**Example Request**:

```json
{
  "source_ulids": [
    "01KHFFS79PW4SBJK3MGAQ8EW9C",
    "01KHFFV3VSRM9YPC4AVSK6K7YY",
    "01KHFFXYZ012345678901234AB"
  ],
  "new_tag": {
    "name": "Frontend Development",
    "color": "#F59E0B"
  }
}
```

#### Response

**Success (200 OK)**:

```json
{
  "status": "success",
  "data": {
    "mergedTags": [
      {
        "ulid": "01KHFFS79PW4SBJK3MGAQ8EW9C",
        "name": "JavaScript",
        "color": "#3B82F6",
        "mergedToUlid": "01KHFG9ZT4K3N8MXJC5W7VKTM9",
        "mergedAt": "2026-02-16T12:00:00Z",
        "createdAt": "2026-01-10T10:00:00Z",
        "updatedAt": "2026-02-16T12:00:00Z"
      },
      {
        "ulid": "01KHFFV3VSRM9YPC4AVSK6K7YY",
        "name": "React",
        "color": "#06B6D4",
        "mergedToUlid": "01KHFG9ZT4K3N8MXJC5W7VKTM9",
        "mergedAt": "2026-02-16T12:00:00Z",
        "createdAt": "2026-01-12T15:00:00Z",
        "updatedAt": "2026-02-16T12:00:00Z"
      },
      {
        "ulid": "01KHFFXYZ012345678901234AB",
        "name": "Vue.js",
        "color": "#10B981",
        "mergedToUlid": "01KHFG9ZT4K3N8MXJC5W7VKTM9",
        "mergedAt": "2026-02-16T12:00:00Z",
        "createdAt": "2026-01-15T08:00:00Z",
        "updatedAt": "2026-02-16T12:00:00Z"
      }
    ],
    "newTag": {
      "ulid": "01KHFG9ZT4K3N8MXJC5W7VKTM9",
      "name": "Frontend Development",
      "color": "#F59E0B",
      "todoCount": 42,
      "completedCount": 18,
      "activeCount": 24,
      "createdAt": "2026-02-16T12:00:00Z",
      "updatedAt": "2026-02-16T12:00:00Z"
    }
  }
}
```

**Response Fields**:

- `mergedTags`: マージされたタグの配列（同上）
- `newTag`: 新しく作成されたタグの情報
  - `ulid`: 新しいタグのULID
  - `name`: タグ名
  - `color`: タグカラー
  - `todoCount`: 関連するTODO数（合計）
  - `completedCount`: 完了TODO数
  - `activeCount`: アクティブTODO数
  - `createdAt`: 作成日時
  - `updatedAt`: 更新日時

**Error Responses**:

**400 Bad Request** - source_ulids未指定:
```json
{
  "status": "error",
  "message": "source_ulids is required"
}
```

**400 Bad Request** - タグ名未指定:
```json
{
  "status": "error",
  "message": "Tag name is required"
}
```

**400 Bad Request** - タグ名が長すぎる:
```json
{
  "status": "error",
  "message": "Tag name must be 50 characters or less"
}
```

**400 Bad Request** - バリデーションエラー:
```json
{
  "status": "error",
  "message": "Validation failed",
  "errors": [
    "Source tag 'JavaScript' does not exist",
    "Tag name 'Frontend' already exists"
  ]
}
```

**401 Unauthorized** - 認証エラー:
```json
{
  "status": "error",
  "message": "Authentication required"
}
```

**403 Forbidden** - 権限エラー:
```json
{
  "status": "error",
  "message": "Access denied: You do not own this tag"
}
```

**500 Internal Server Error** - サーバーエラー:
```json
{
  "status": "error",
  "message": "Internal server error: <error details>"
}
```

---

## Data Types

### Tag

| Field | Type | Description |
|-------|------|-------------|
| ulid | string | タグのULID（26文字） |
| name | string | タグ名（1-50文字） |
| color | string | 16進数カラーコード（例: #3B82F6） |
| mergedToUlid | string \| null | マージ先タグのULID |
| mergedAt | string \| null | マージ日時（ISO 8601形式） |
| createdAt | string | 作成日時（ISO 8601形式） |
| updatedAt | string | 更新日時（ISO 8601形式） |

### Tag with Statistics

Tag型の全フィールドに加えて：

| Field | Type | Description |
|-------|------|-------------|
| todoCount | number | 関連するTODO数（合計） |
| completedCount | number | 完了TODO数 |
| activeCount | number | アクティブTODO数 |

---

## Business Rules

### Merge Operation Rules

1. **所有者制限**: ユーザーは自分が所有するタグのみマージ可能
2. **重複防止**: 同じTODOまたはラベルに同じタグが複数付かないよう自動的にチェック
3. **不可逆性**: マージ操作は元に戻せない
4. **マージチェーン**: マージ先タグが既にマージ済みの場合、最終的なマージ先を自動的に解決（最大10階層）
5. **トランザクション**: すべての操作はトランザクション内で実行され、エラー時はロールバック

### Validation Rules

#### Source Tags
- 最低1つのソースタグが必要
- すべてのソースタグが存在する必要がある
- すべてのソースタグがユーザーに所有されている必要がある
- ソースタグはマージ先タグと異なる必要がある

#### Target Tag (既存タグへのマージ)
- ターゲットタグが存在する必要がある
- ターゲットタグがユーザーに所有されている必要がある
- ターゲットタグはソースタグのいずれとも異なる必要がある

#### New Tag (新規タグへのマージ)
- タグ名は必須（1-50文字）
- タグ名は既存のタグと重複しない必要がある
- カラーコードは有効な16進数形式である必要がある（任意）

---

## Error Handling

### Error Response Format

すべてのエラーレスポンスは以下の形式に従います：

```json
{
  "status": "error",
  "message": "<error message>",
  "errors": ["<detailed error 1>", "<detailed error 2>"]
}
```

`errors`フィールドは複数のバリデーションエラーがある場合にのみ含まれます。

### HTTP Status Codes

| Status Code | Description |
|-------------|-------------|
| 200 | 成功 |
| 400 | リクエストが不正（バリデーションエラー） |
| 401 | 認証が必要 |
| 403 | アクセス権限がない |
| 404 | リソースが見つからない |
| 500 | サーバー内部エラー |

---

## Rate Limiting

現在、Rate Limitingは実装されていません。将来的には以下のような制限が追加される可能性があります：

- ユーザーあたり: 100リクエスト/分
- IPアドレスあたり: 300リクエスト/分

---

## Examples

### Example 1: 2つのタグを既存タグにマージ

**Request**:
```bash
curl -X POST http://localhost:5000/api/v1/tags/merge \
  -H "Content-Type: application/json" \
  -H "Cookie: session_id=abc123..." \
  -d '{
    "source_ulids": ["01KHFFS79PW4SBJK3MGAQ8EW9C", "01KHFFV3VSRM9YPC4AVSK6K7YY"],
    "target_ulid": "01KHFG2HP5E8N9QXJC5W7VKTM3"
  }'
```

**Response**:
```json
{
  "status": "success",
  "data": {
    "mergedTags": [...],
    "targetTag": {...}
  }
}
```

### Example 2: 3つのタグを新規タグにマージ

**Request**:
```bash
curl -X POST http://localhost:5000/api/v1/tags/merge-to-new \
  -H "Content-Type: application/json" \
  -H "Cookie: session_id=abc123..." \
  -d '{
    "source_ulids": [
      "01KHFFS79PW4SBJK3MGAQ8EW9C",
      "01KHFFV3VSRM9YPC4AVSK6K7YY",
      "01KHFFXYZ012345678901234AB"
    ],
    "new_tag": {
      "name": "Frontend Development",
      "color": "#F59E0B"
    }
  }'
```

**Response**:
```json
{
  "status": "success",
  "data": {
    "mergedTags": [...],
    "newTag": {...}
  }
}
```

### Example 3: バリデーションエラー

**Request**:
```bash
curl -X POST http://localhost:5000/api/v1/tags/merge \
  -H "Content-Type: application/json" \
  -H "Cookie: session_id=abc123..." \
  -d '{
    "source_ulids": [],
    "target_ulid": "01KHFG2HP5E8N9QXJC5W7VKTM3"
  }'
```

**Response**:
```json
{
  "status": "error",
  "message": "source_ulids is required"
}
```

---

## Testing

### Testing with cURL

マージ機能のテストには以下のcURLコマンドを使用できます：

```bash
# 1. ログインしてセッションIDを取得
SESSION_ID=$(curl -X POST http://localhost:5000/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"user@example.com","password":"password"}' \
  -c - | grep session_id | awk '{print $7}')

# 2. タグをマージ
curl -X POST http://localhost:5000/api/v1/tags/merge \
  -H "Content-Type: application/json" \
  -H "Cookie: session_id=$SESSION_ID" \
  -d '{
    "source_ulids": ["01KHFFS79PW4SBJK3MGAQ8EW9C"],
    "target_ulid": "01KHFG2HP5E8N9QXJC5W7VKTM3"
  }'
```

---

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-02-16 | 1.0.0 | Initial API documentation | AI Assistant |

---

## Support

問題が発生した場合は、以下を確認してください：

1. **認証**: 有効なセッションIDを持っているか
2. **リクエスト形式**: Content-Typeヘッダーが正しく設定されているか
3. **パラメータ**: 必須パラメータがすべて含まれているか
4. **権限**: 操作しようとしているタグを所有しているか

それでも問題が解決しない場合は、サーバーログを確認してください。
