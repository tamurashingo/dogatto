# API エラーレスポンス統一仕様

## 概要

DOGATTO システムで使用するREST APIのエラーレスポンス形式を統一し、クライアント側での一貫したエラーハンドリングを可能にする。

## 基本方針

### 1. 一貫性
- すべてのAPIで同じエラーフォーマットを使用
- HTTPステータスコードとエラーコードの組み合わせで詳細を表現
- 多言語対応を考慮した設計

### 2. 開発者体験
- エラーの原因がすぐに分かる
- デバッグに必要な情報を含める
- クライアント側で適切なエラー処理を実装しやすい

### 3. セキュリティ
- 内部実装の詳細を漏らさない
- 本番環境と開発環境で出力レベルを調整
- 機密情報（パスワード、トークンなど）を含めない

## エラーレスポンス形式

### 基本構造

```json
{
  "status": "error",
  "error": {
    "code": "ERROR_CODE",
    "message": "ユーザー向けエラーメッセージ",
    "details": {
      "field": ["フィールド固有のエラーメッセージ"]
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### フィールド説明

#### status (string, required)
- **値**: `"error"` 固定
- **用途**: レスポンスがエラーであることを示す
- **成功時**: `"success"`

#### error.code (string, required)
- **形式**: `UPPER_SNAKE_CASE`
- **用途**: エラーの種類を識別するコード
- **例**: `VALIDATION_ERROR`, `UNAUTHORIZED`, `RESOURCE_NOT_FOUND`

#### error.message (string, required)
- **内容**: エンドユーザー向けのエラーメッセージ
- **言語**: 日本語（将来的には多言語対応）
- **長さ**: 200文字以内
- **例**: "入力内容に誤りがあります"

#### error.details (object, optional)
- **内容**: エラーの詳細情報
- **用途**: バリデーションエラーなど、複数のエラーを返す場合
- **構造**: `{ "フィールド名": ["エラーメッセージ配列"] }`

#### error.request_id (string, required)
- **形式**: ULID
- **用途**: リクエストの追跡、ログとの紐付け
- **長さ**: 26文字

#### error.timestamp (string, required)
- **形式**: ISO 8601（UTC）
- **例**: `2026-01-11T12:00:00Z`
- **用途**: エラー発生時刻の記録

### 開発環境専用フィールド

本番環境では含めない、開発環境のみで出力するフィールド：

```json
{
  "status": "error",
  "error": {
    "code": "INTERNAL_SERVER_ERROR",
    "message": "サーバーエラーが発生しました",
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z",
    "debug": {
      "stack_trace": "Error: ...\n  at ...",
      "query": "SELECT * FROM users WHERE id = ?",
      "params": [123]
    }
  }
}
```

## HTTPステータスコード

### 2xx 成功
- **200 OK**: リクエスト成功、レスポンスボディあり
- **201 Created**: リソース作成成功
- **204 No Content**: リクエスト成功、レスポンスボディなし

### 4xx クライアントエラー

#### 400 Bad Request
**用途**: リクエストの形式が不正

**エラーコード:**
- `VALIDATION_ERROR`: バリデーションエラー
- `INVALID_REQUEST`: リクエスト形式が不正
- `INVALID_JSON`: JSONパースエラー

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "入力内容に誤りがあります",
    "details": {
      "email": ["メールアドレスの形式が正しくありません"],
      "password": ["パスワードは8文字以上で入力してください"]
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 401 Unauthorized
**用途**: 認証が必要、または認証失敗

**エラーコード:**
- `UNAUTHORIZED`: 認証が必要
- `INVALID_CREDENTIALS`: 認証情報が不正
- `TOKEN_EXPIRED`: トークンの有効期限切れ
- `TOKEN_INVALID`: トークンが不正

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "INVALID_CREDENTIALS",
    "message": "メールアドレスまたはパスワードが正しくありません",
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 403 Forbidden
**用途**: 認証済みだが権限がない

**エラーコード:**
- `FORBIDDEN`: アクセス権限なし
- `INSUFFICIENT_PERMISSION`: 権限不足
- `ACCOUNT_SUSPENDED`: アカウント停止中
- `EMAIL_NOT_VERIFIED`: メール未認証

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "EMAIL_NOT_VERIFIED",
    "message": "メールアドレスの認証が完了していません",
    "details": {
      "verification_url": "/verify-email/resend"
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 404 Not Found
**用途**: リソースが存在しない

**エラーコード:**
- `RESOURCE_NOT_FOUND`: リソースが見つからない
- `TODO_NOT_FOUND`: TODOが見つからない
- `TAG_NOT_FOUND`: タグが見つからない
- `LABEL_NOT_FOUND`: ラベルが見つからない
- `USER_NOT_FOUND`: ユーザーが見つからない

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "TODO_NOT_FOUND",
    "message": "指定されたTODOが見つかりません",
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 409 Conflict
**用途**: リソースの競合

**エラーコード:**
- `DUPLICATE_RESOURCE`: リソースが既に存在
- `DUPLICATE_EMAIL`: メールアドレスが既に登録済み
- `DUPLICATE_TAG`: タグが既に存在
- `DUPLICATE_LABEL`: ラベルが既に存在
- `CONFLICT`: 競合エラー

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "DUPLICATE_TAG",
    "message": "このタグは既に存在します",
    "details": {
      "tag_name": "MORNING",
      "existing_tag_ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 422 Unprocessable Entity
**用途**: ビジネスロジックのバリデーションエラー

**エラーコード:**
- `BUSINESS_RULE_VIOLATION`: ビジネスルール違反
- `INVALID_STATE_TRANSITION`: 状態遷移が不正
- `MERGED_TAG`: マージ済みタグの操作
- `MERGED_LABEL`: マージ済みラベルの操作

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "MERGED_TAG",
    "message": "このタグは既にマージされています",
    "details": {
      "merged_to_ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV",
      "merged_at": "2026-01-10T10:00:00Z"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 429 Too Many Requests
**用途**: レート制限超過

**エラーコード:**
- `RATE_LIMIT_EXCEEDED`: レート制限超過

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "リクエストが多すぎます。しばらくしてから再度お試しください",
    "details": {
      "retry_after": 60,
      "limit": 10,
      "window": "1m"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

**ヘッダー:**
```
Retry-After: 60
X-RateLimit-Limit: 10
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 1704974460
```

### 5xx サーバーエラー

#### 500 Internal Server Error
**用途**: サーバー内部エラー

**エラーコード:**
- `INTERNAL_SERVER_ERROR`: 内部サーバーエラー
- `DATABASE_ERROR`: データベースエラー
- `EXTERNAL_SERVICE_ERROR`: 外部サービスエラー

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "INTERNAL_SERVER_ERROR",
    "message": "サーバーエラーが発生しました。しばらくしてから再度お試しください",
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 502 Bad Gateway
**用途**: 外部サービスからの不正なレスポンス

**エラーコード:**
- `BAD_GATEWAY`: ゲートウェイエラー

#### 503 Service Unavailable
**用途**: サービス一時停止

**エラーコード:**
- `SERVICE_UNAVAILABLE`: サービス利用不可
- `MAINTENANCE`: メンテナンス中

**例:**
```json
{
  "status": "error",
  "error": {
    "code": "MAINTENANCE",
    "message": "現在メンテナンス中です。1時間後に再開予定です",
    "details": {
      "estimated_recovery": "2026-01-11T13:00:00Z"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

#### 504 Gateway Timeout
**用途**: タイムアウト

**エラーコード:**
- `GATEWAY_TIMEOUT`: ゲートウェイタイムアウト

## エラーコード一覧

### 認証・認可

| コード | HTTPステータス | 説明 |
|--------|---------------|------|
| `UNAUTHORIZED` | 401 | 認証が必要 |
| `INVALID_CREDENTIALS` | 401 | 認証情報が不正 |
| `TOKEN_EXPIRED` | 401 | トークンの有効期限切れ |
| `TOKEN_INVALID` | 401 | トークンが不正 |
| `FORBIDDEN` | 403 | アクセス権限なし |
| `INSUFFICIENT_PERMISSION` | 403 | 権限不足 |
| `ACCOUNT_SUSPENDED` | 403 | アカウント停止中 |
| `EMAIL_NOT_VERIFIED` | 403 | メール未認証 |

### バリデーション

| コード | HTTPステータス | 説明 |
|--------|---------------|------|
| `VALIDATION_ERROR` | 400 | バリデーションエラー |
| `INVALID_REQUEST` | 400 | リクエスト形式が不正 |
| `INVALID_JSON` | 400 | JSONパースエラー |
| `INVALID_PARAMETER` | 400 | パラメータが不正 |
| `MISSING_PARAMETER` | 400 | 必須パラメータがない |
| `INVALID_FORMAT` | 400 | フォーマットが不正 |

### リソース

| コード | HTTPステータス | 説明 |
|--------|---------------|------|
| `RESOURCE_NOT_FOUND` | 404 | リソースが見つからない |
| `TODO_NOT_FOUND` | 404 | TODOが見つからない |
| `TAG_NOT_FOUND` | 404 | タグが見つからない |
| `LABEL_NOT_FOUND` | 404 | ラベルが見つからない |
| `USER_NOT_FOUND` | 404 | ユーザーが見つからない |
| `COMMENT_NOT_FOUND` | 404 | コメントが見つからない |

### 競合

| コード | HTTPステータス | 説明 |
|--------|---------------|------|
| `DUPLICATE_RESOURCE` | 409 | リソースが既に存在 |
| `DUPLICATE_EMAIL` | 409 | メールアドレスが既に登録済み |
| `DUPLICATE_USERNAME` | 409 | ユーザー名が既に使用されている |
| `DUPLICATE_TAG` | 409 | タグが既に存在 |
| `DUPLICATE_LABEL` | 409 | ラベルが既に存在 |
| `CONFLICT` | 409 | 競合エラー |

### ビジネスロジック

| コード | HTTPステータス | 説明 |
|--------|---------------|------|
| `BUSINESS_RULE_VIOLATION` | 422 | ビジネスルール違反 |
| `INVALID_STATE_TRANSITION` | 422 | 状態遷移が不正 |
| `MERGED_TAG` | 422 | マージ済みタグの操作 |
| `MERGED_LABEL` | 422 | マージ済みラベルの操作 |
| `CANNOT_DELETE_USED_RESOURCE` | 422 | 使用中のリソースは削除不可 |
| `MINIMUM_TAGS_REQUIRED` | 422 | 最低1つのタグが必要 |

### レート制限

| コード | HTTPステータス | 説明 |
|--------|---------------|------|
| `RATE_LIMIT_EXCEEDED` | 429 | レート制限超過 |

### サーバーエラー

| コード | HTTPステータス | 説明 |
|--------|---------------|------|
| `INTERNAL_SERVER_ERROR` | 500 | 内部サーバーエラー |
| `DATABASE_ERROR` | 500 | データベースエラー |
| `EXTERNAL_SERVICE_ERROR` | 500 | 外部サービスエラー |
| `BAD_GATEWAY` | 502 | ゲートウェイエラー |
| `SERVICE_UNAVAILABLE` | 503 | サービス利用不可 |
| `MAINTENANCE` | 503 | メンテナンス中 |
| `GATEWAY_TIMEOUT` | 504 | ゲートウェイタイムアウト |

## バリデーションエラーの詳細形式

### 単一フィールドエラー

```json
{
  "status": "error",
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "入力内容に誤りがあります",
    "details": {
      "email": ["メールアドレスの形式が正しくありません"]
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 複数フィールドエラー

```json
{
  "status": "error",
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "入力内容に誤りがあります",
    "details": {
      "email": [
        "メールアドレスの形式が正しくありません",
        "メールアドレスは必須です"
      ],
      "password": [
        "パスワードは8文字以上で入力してください"
      ],
      "username": [
        "ユーザー名は必須です"
      ]
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### ネストしたフィールドエラー

```json
{
  "status": "error",
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "入力内容に誤りがあります",
    "details": {
      "tags[0].name": ["タグ名は必須です"],
      "tags[1].color": ["色の形式が正しくありません"]
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

## 具体的なエラーレスポンス例

### 1. ログイン失敗

```json
{
  "status": "error",
  "error": {
    "code": "INVALID_CREDENTIALS",
    "message": "メールアドレスまたはパスワードが正しくありません",
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 2. ユーザー登録（重複メールアドレス）

```json
{
  "status": "error",
  "error": {
    "code": "DUPLICATE_EMAIL",
    "message": "このメールアドレスは既に登録されています",
    "details": {
      "email": "user@example.com"
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 3. TODO作成（バリデーションエラー）

```json
{
  "status": "error",
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "入力内容に誤りがあります",
    "details": {
      "title": ["件名は必須です"],
      "due_date": ["期限日の形式が正しくありません"],
      "tags": ["タグを少なくとも1つ選択してください"]
    },
    "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 4. TODOが見つからない

```json
{
  "status": "error",
  "error": {
    "code": "TODO_NOT_FOUND",
    "message": "指定されたTODOが見つかりません",
    "details": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 5. タグマージ（既にマージ済み）

```json
{
  "status": "error",
  "error": {
    "code": "MERGED_TAG",
    "message": "このタグは既にマージされています",
    "details": {
      "tag_ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "merged_to_ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV",
      "merged_to_name": "MORNING",
      "merged_at": "2026-01-10T10:00:00Z"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 6. レート制限超過

```json
{
  "status": "error",
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "リクエストが多すぎます。60秒後に再度お試しください",
    "details": {
      "retry_after": 60,
      "limit": 10,
      "window": "1m",
      "current": 11
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 7. メール未認証

```json
{
  "status": "error",
  "error": {
    "code": "EMAIL_NOT_VERIFIED",
    "message": "メールアドレスの認証が完了していません",
    "details": {
      "email": "user@example.com",
      "verification_sent_at": "2026-01-11T11:00:00Z",
      "can_resend": true
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 8. 権限不足

```json
{
  "status": "error",
  "error": {
    "code": "FORBIDDEN",
    "message": "このリソースにアクセスする権限がありません",
    "details": {
      "resource_type": "todo",
      "resource_ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "required_permission": "write"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 9. サーバーエラー

```json
{
  "status": "error",
  "error": {
    "code": "INTERNAL_SERVER_ERROR",
    "message": "サーバーエラーが発生しました。しばらくしてから再度お試しください",
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

### 10. メンテナンス中

```json
{
  "status": "error",
  "error": {
    "code": "MAINTENANCE",
    "message": "現在メンテナンス中です",
    "details": {
      "maintenance_started_at": "2026-01-11T12:00:00Z",
      "estimated_recovery": "2026-01-11T13:00:00Z",
      "message": "定期メンテナンスを実施しています"
    },
    "request_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
    "timestamp": "2026-01-11T12:00:00Z"
  }
}
```

## クライアント側の実装

### TypeScript 型定義

```typescript
// エラーレスポンスの型定義
interface ApiErrorResponse {
  status: 'error';
  error: {
    code: string;
    message: string;
    details?: Record<string, string[]>;
    request_id: string;
    timestamp: string;
    debug?: {
      stack_trace?: string;
      query?: string;
      params?: any[];
    };
  };
}

// 成功レスポンスの型定義
interface ApiSuccessResponse<T> {
  status: 'success';
  data: T;
}

// APIレスポンスの型
type ApiResponse<T> = ApiSuccessResponse<T> | ApiErrorResponse;
```

### エラーハンドリングの例

```typescript
// エラーハンドリングクラス
class ApiError extends Error {
  constructor(
    public code: string,
    public message: string,
    public details?: Record<string, string[]>,
    public requestId?: string,
    public timestamp?: string
  ) {
    super(message);
    this.name = 'ApiError';
  }

  // バリデーションエラーかどうか
  isValidationError(): boolean {
    return this.code === 'VALIDATION_ERROR';
  }

  // 認証エラーかどうか
  isAuthError(): boolean {
    return ['UNAUTHORIZED', 'INVALID_CREDENTIALS', 'TOKEN_EXPIRED', 'TOKEN_INVALID'].includes(this.code);
  }

  // リソースが見つからないエラーかどうか
  isNotFoundError(): boolean {
    return this.code.endsWith('_NOT_FOUND');
  }

  // レート制限エラーかどうか
  isRateLimitError(): boolean {
    return this.code === 'RATE_LIMIT_EXCEEDED';
  }

  // サーバーエラーかどうか
  isServerError(): boolean {
    return this.code === 'INTERNAL_SERVER_ERROR' || this.code === 'DATABASE_ERROR';
  }
}

// APIクライアント
async function apiCall<T>(url: string, options?: RequestInit): Promise<T> {
  try {
    const response = await fetch(url, options);
    const data: ApiResponse<T> = await response.json();

    if (data.status === 'error') {
      throw new ApiError(
        data.error.code,
        data.error.message,
        data.error.details,
        data.error.request_id,
        data.error.timestamp
      );
    }

    return data.data;
  } catch (error) {
    if (error instanceof ApiError) {
      throw error;
    }
    // ネットワークエラーなど
    throw new ApiError('NETWORK_ERROR', 'ネットワークエラーが発生しました');
  }
}

// 使用例
try {
  const todo = await apiCall<Todo>('/api/todos/01ARZ3NDEKTSV4RRFFQ69G5FAV');
  console.log(todo);
} catch (error) {
  if (error instanceof ApiError) {
    if (error.isValidationError()) {
      // バリデーションエラーの処理
      console.error('Validation errors:', error.details);
    } else if (error.isAuthError()) {
      // 認証エラーの処理
      window.location.href = '/login';
    } else if (error.isNotFoundError()) {
      // リソースが見つからない
      console.error('Resource not found');
    } else {
      // その他のエラー
      console.error('Error:', error.message);
    }
  }
}
```

### React でのエラーハンドリング

```typescript
// エラー表示コンポーネント
function ErrorMessage({ error }: { error: ApiError }) {
  if (error.isValidationError() && error.details) {
    return (
      <div className="error-messages">
        {Object.entries(error.details).map(([field, messages]) => (
          <div key={field} className="field-error">
            <strong>{field}:</strong>
            <ul>
              {messages.map((msg, i) => (
                <li key={i}>{msg}</li>
              ))}
            </ul>
          </div>
        ))}
      </div>
    );
  }

  return <div className="error-message">{error.message}</div>;
}

// カスタムフック
function useTodoCreate() {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<ApiError | null>(null);

  const createTodo = async (data: CreateTodoRequest) => {
    setLoading(true);
    setError(null);

    try {
      const todo = await apiCall<Todo>('/api/todos', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(data),
      });
      return todo;
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err);
      }
      throw err;
    } finally {
      setLoading(false);
    }
  };

  return { createTodo, loading, error };
}
```

## サーバー側の実装

### Common Lisp での実装例

```lisp
;; エラーレスポンス生成関数
(defun make-error-response (status-code error-code message &key details debug)
  "エラーレスポンスを生成"
  (setf (lack.response:response-status *response*) status-code)
  (setf (lack.response:response-headers *response*)
        (append (lack.response:response-headers *response*)
                '(:content-type "application/json; charset=utf-8")))
  (let ((error-response
         `((:status . "error")
           (:error . ((:code . ,error-code)
                      (:message . ,message)
                      ,@(when details `((:details . ,details)))
                      (:request--id . ,(generate-ulid))
                      (:timestamp . ,(local-time:format-timestring
                                      nil
                                      (local-time:now)
                                      :format local-time:+iso-8601-format+))
                      ,@(when (and debug (not (production-p)))
                          `((:debug . ,debug))))))))
    (jojo:to-json error-response)))

;; バリデーションエラー
(defun validation-error (details)
  "バリデーションエラーのレスポンスを生成"
  (make-error-response 400 "VALIDATION_ERROR" "入力内容に誤りがあります"
                       :details details))

;; リソースが見つからないエラー
(defun not-found-error (resource-type ulid)
  "リソースが見つからないエラーのレスポンスを生成"
  (make-error-response 404
                       (format nil "~A_NOT_FOUND" (string-upcase resource-type))
                       (format nil "指定された~Aが見つかりません" resource-type)
                       :details `((:ulid . ,ulid))))

;; 重複エラー
(defun duplicate-error (resource-type name)
  "重複エラーのレスポンスを生成"
  (make-error-response 409
                       (format nil "DUPLICATE_~A" (string-upcase resource-type))
                       (format nil "この~Aは既に存在します" resource-type)
                       :details `((,(intern (string-downcase (format nil "~A_name" resource-type)) :keyword)
                                   . ,name))))

;; 認証エラー
(defun unauthorized-error ()
  "認証エラーのレスポンスを生成"
  (make-error-response 401 "UNAUTHORIZED" "認証が必要です"))

;; 権限エラー
(defun forbidden-error (message)
  "権限エラーのレスポンスを生成"
  (make-error-response 403 "FORBIDDEN" message))

;; サーバーエラー
(defun internal-server-error (&optional (condition nil))
  "サーバーエラーのレスポンスを生成"
  (make-error-response 500
                       "INTERNAL_SERVER_ERROR"
                       "サーバーエラーが発生しました。しばらくしてから再度お試しください"
                       :debug (when condition
                                `((:stack--trace . ,(princ-to-string condition))))))

;; 使用例
(defun get-todo (ulid)
  "TODOを取得"
  (let ((todo (find-todo-by-ulid ulid)))
    (if todo
        (jojo:to-json `((:status . "success")
                        (:data . ((:todo . ,todo)))))
        (not-found-error "TODO" ulid))))
```

## ロギング

### ログ出力項目

すべてのエラーについて以下の情報をログに出力する：

```json
{
  "level": "error",
  "timestamp": "2026-01-11T12:00:00Z",
  "request_id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "user_id": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
  "method": "POST",
  "path": "/api/todos",
  "status_code": 400,
  "error_code": "VALIDATION_ERROR",
  "message": "入力内容に誤りがあります",
  "details": {
    "title": ["件名は必須です"]
  },
  "stack_trace": "Error: ...\n  at ...",
  "user_agent": "Mozilla/5.0...",
  "ip_address": "192.168.1.1"
}
```

### ログレベル

- **ERROR**: 4xx エラー（クライアントエラー）
- **FATAL**: 5xx エラー（サーバーエラー）
- **WARN**: レート制限エラー
- **INFO**: 認証失敗（セキュリティ監視用）

## セキュリティ考慮事項

### 1. 情報漏洩の防止

**本番環境では含めない情報:**
- スタックトレース
- SQLクエリ
- データベースの構造
- 内部パス
- システムの詳細

**開発環境のみ:**
- `debug` フィールドで詳細情報を提供

### 2. 認証エラーの取り扱い

**悪い例:**
```json
{
  "code": "USER_NOT_FOUND",
  "message": "このメールアドレスは登録されていません"
}
```
→ メールアドレスの存在確認に悪用される

**良い例:**
```json
{
  "code": "INVALID_CREDENTIALS",
  "message": "メールアドレスまたはパスワードが正しくありません"
}
```
→ ユーザーの存在を推測できない

### 3. エラーメッセージの注意点

- 機密情報を含めない
- 内部実装の詳細を漏らさない
- ユーザーに有用な情報のみ提供

## テスト

### エラーレスポンスのテスト

```lisp
;; バリデーションエラーのテスト
(deftest test-validation-error ()
  (let ((response (validation-error '((:email . ("メールアドレスの形式が正しくありません"))))))
    (is (equal (jsown:val (jsown:parse response) "status") "error"))
    (is (equal (jsown:val (jsown:parse response) "error" "code") "VALIDATION_ERROR"))
    (is (not (null (jsown:val (jsown:parse response) "error" "request_id"))))
    (is (not (null (jsown:val (jsown:parse response) "error" "timestamp"))))))

;; 404エラーのテスト
(deftest test-not-found-error ()
  (let ((response (not-found-error "TODO" "01ARZ3NDEKTSV4RRFFQ69G5FAV")))
    (is (equal (jsown:val (jsown:parse response) "status") "error"))
    (is (equal (jsown:val (jsown:parse response) "error" "code") "TODO_NOT_FOUND"))))
```

## まとめ

### 実装チェックリスト

- [ ] エラーレスポンス生成関数の実装
- [ ] HTTPステータスコードの適切な設定
- [ ] エラーコードの定義
- [ ] バリデーションエラーの詳細出力
- [ ] request_id の生成と出力
- [ ] タイムスタンプの出力
- [ ] ログ出力の実装
- [ ] 開発環境と本番環境の切り替え
- [ ] クライアント側のエラーハンドリング実装
- [ ] エラーレスポンスのテスト

## 関連仕様

- プロジェクト憲章: `constitution.md`
- データベーススキーマ: `database-schema-spec.md`
- タグ管理画面仕様: `tags-management-spec.md`
- ラベル管理画面仕様: `labels-management-spec.md`
- ログイン画面仕様: `login-spec.md`
- ユーザー登録画面仕様: `signup-spec.md`

---

**Version**: 1.0.0
**Created**: 2026-01-11
**Last Updated**: 2026-01-11
