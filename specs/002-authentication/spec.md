# 002-authentication: ユーザー認証システム

## 概要

DOGATTOのユーザー認証システムの仕様を定義します。ユーザー登録、ログイン、ログアウト、セッション管理を含みます。

## 目的

1. メール/パスワードによる安全なユーザー登録を実装
2. ログイン/ログアウト機能を実装
3. Redisを使用したユーザーセッション管理
4. 認証が必要なルートを保護
5. 認証UIコンポーネントの提供

## スコープ

### 対象範囲

- ユーザー登録（メール + パスワード）
- セッション作成を伴うユーザーログイン
- セッション破棄を伴うユーザーログアウト
- パスワードハッシュ化（bcrypt）
- Redisへのセッション保存
- 認証ミドルウェア
- フロントエンドのログイン/登録ページ
- 保護されたルート
- 現在のユーザーAPI

### 対象外（将来の拡張機能）

- ソーシャルログイン（Google、GitHubなど）
- 二要素認証（2FA）
- メールによるパスワードリセット
- メール認証
- ログイン状態の保持機能
- OAuth2/OpenID Connect

## ユーザーストーリー

### US5: ユーザー登録

**ユーザーとして**：新規ユーザー  
**実現したいこと**：メールアドレスとパスワードでアカウントを作成  
**理由**：TODOマネジメントシステムにアクセスするため

**受け入れ基準：**
- メールアドレスとパスワードで登録できる
- メールアドレスは一意である必要がある
- パスワードはセキュリティ要件を満たす必要がある（最低8文字）
- パスワードは保存前にハッシュ化される
- 適切なエラーメッセージが表示される
- 登録成功後はログインページにリダイレクトされる

### US6: ユーザーログイン

**ユーザーとして**：登録済みユーザー  
**実現したいこと**：認証情報でログイン  
**理由**：自分のTODOにアクセスするため

**受け入れ基準：**
- メールアドレスとパスワードでログインできる
- 無効な認証情報の場合はエラーメッセージが表示される
- ログイン成功時にセッションが作成される
- セッションはRedisに保存される
- ログイン後はTODO一覧にリダイレクトされる
- ページをリロードしてもセッションは維持される

### US7: ユーザーログアウト

**ユーザーとして**：ログイン中のユーザー  
**実現したいこと**：ログアウト  
**理由**：アカウントを保護するため

**受け入れ基準：**
- どのページからでもログアウトできる
- ログアウト時にセッションが破棄される
- ログインページにリダイレクトされる
- ログアウト後に保護されたルートへアクセスするとログインページにリダイレクトされる

### US8: 保護されたルート

**ユーザーとして**：開発者  
**実現したいこと**：認証が必要なルートを保護  
**理由**：未認証ユーザーがプライベートデータにアクセスできないようにするため

**受け入れ基準：**
- 保護されたルートへの未認証リクエストは401を返す
- フロントエンドは保護されたルートに対してログインページにリダイレクトする
- 認証済みリクエストには現在のユーザー情報が利用可能

## 技術仕様

### データベーススキーマ

既に存在（001-initial-setupで作成済み）：
- id (ULID)
- username
- email
- password-hash
- created-at
- updated-at

### APIエンドポイント

#### POST /api/v1/auth/register

新しいユーザーを登録する。

**リクエスト：**
```json
{
  "username": "john_doe",
  "email": "john@example.com",
  "password": "SecurePass123"
}
```

**レスポンス（201 Created）：**
```json
{
  "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "username": "john_doe",
  "email": "john@example.com",
  "createdAt": "2026-01-12T10:00:00Z"
}
```

**エラー：**
- 400: 不正な入力
- 422: バリデーションエラー（メール重複、弱いパスワード）

#### POST /api/v1/auth/login

ユーザーを認証してセッションを作成する。

**リクエスト：**
```json
{
  "email": "john@example.com",
  "password": "SecurePass123"
}
```

**レスポンス（200 OK）：**
```json
{
  "user": {
    "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "username": "john_doe",
    "email": "john@example.com"
  }
}
```

セッションCookieを設定：`session-id=<uuid>; HttpOnly; Secure; SameSite=Lax`

**エラー：**
- 401: 認証情報が無効
- 400: 不正な入力

#### POST /api/v1/auth/logout

現在のセッションを破棄する。

**レスポンス（204 No Content）**

セッションCookieをクリアする。

#### GET /api/v1/auth/me

現在の認証済みユーザーを取得する。

**レスポンス（200 OK）：**
```json
{
  "id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "username": "john_doe",
  "email": "john@example.com",
  "createdAt": "2026-01-12T10:00:00Z"
}
```

**エラー：**
- 401: 未認証

### セッション管理

**ストレージ：** Redis

**キーフォーマット：** `session:<session-id>`

**セッションデータ：**
```lisp
{
  "user-id": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
  "username": "john_doe",
  "email": "john@example.com",
  "created-at": 1640995200,
  "expires-at": 1641081600
}
```

**セッションTTL：** 24時間（設定可能）

**Cookie設定：**
- 名前：`session-id`
- HttpOnly: true
- Secure: true（本番環境）
- SameSite: Lax
- Max-Age: 86400（24時間）

### パスワードセキュリティ

**ハッシュアルゴリズム：** bcrypt

**実装例：**
```lisp
(defun hash-password (password)
  "bcryptを使用してパスワードをハッシュ化する。"
  (ironclad:pbkdf2-hash-password
    (babel:string-to-octets password)
    :salt (ironclad:make-random-salt)
    :digest :sha256
    :iterations 10000))

(defun verify-password (password hash)
  "パスワードをbcryptハッシュと照合する。"
  (ironclad:pbkdf2-check-password
    (babel:string-to-octets password)
    hash))
```

**パスワード要件：**
- 最低8文字
- 少なくとも1つの文字
- 少なくとも1つの数字

### フロントエンドコンポーネント

#### LoginPage

- メールアドレス入力
- パスワード入力
- 送信ボタン
- 登録ページへのリンク
- エラーメッセージ表示

#### RegisterPage

- ユーザー名入力
- メールアドレス入力
- パスワード入力
- パスワード確認入力
- 送信ボタン
- ログインページへのリンク
- エラーメッセージ表示
- バリデーションフィードバック

#### ProtectedRoute

認証が必要なルートをラップするHOC。

```typescript
<ProtectedRoute>
  <TodoListPage />
</ProtectedRoute>
```

未認証の場合は `/login` にリダイレクト。

### ミドルウェア

#### 認証ミドルウェア

```lisp
(defun require-authentication (handler)
  "ユーザーが認証されている必要があるミドルウェア。"
  (lambda (env)
    (let ((session-id (get-session-id env)))
      (if (and session-id (session-valid? session-id))
          (funcall handler (add-user-to-env env session-id))
          (unauthorized-response)))))
```

## セキュリティ考慮事項

1. **パスワード保存**
   - 平文パスワードを保存しない
   - 適切な反復回数でbcryptを使用
   - ソルトはbcryptが自動的に処理

2. **セッションセキュリティ**
   - HttpOnly Cookieでクロスサイトスクリプティング（XSS）を防止
   - HTTPSのためのSecureフラグ
   - SameSiteでクロスサイトリクエストフォージェリ（CSRF）を防止
   - セッション有効期限

3. **入力バリデーション**
   - メールフォーマットの検証
   - パスワード要件の強制
   - すべての入力をサニタイズ
   - SQLインジェクションの防止（パラメータ化クエリ）

4. **レート制限**（将来）
   - ログイン試行回数の制限
   - アカウントロックアウトの実装

5. **HTTPS専用**（本番環境）
   - すべての認証エンドポイントはHTTPSを使用
   - セキュアCookieはHTTPS経由でのみ動作

## テスト戦略

### バックエンドテスト

- 有効なデータでのユーザー登録
- 重複メールでのユーザー登録
- 無効なデータでのユーザー登録
- 有効な認証情報でのログイン
- 無効な認証情報でのログイン
- ログアウトによるセッション破棄
- 保護されたエンドポイントが認証を要求
- セッションの有効期限

### フロントエンドテスト

- ログインフォームのバリデーション
- 登録フォームのバリデーション
- ログイン成功時のダッシュボードへのリダイレクト
- ログイン失敗時のエラー表示
- ログアウト時のログインページへのリダイレクト
- 未認証時の保護されたルートへのリダイレクト

## 成功基準

- [ ] ユーザーがメール/パスワードで登録できる
- [ ] ユーザーがログインできる
- [ ] ユーザーがログアウトできる
- [ ] セッションがRedisに保存される
- [ ] セッションが24時間後に期限切れになる
- [ ] パスワードが安全にハッシュ化される
- [ ] 保護されたルートが認証を要求する
- [ ] フロントエンドが適切なエラーメッセージを表示する
- [ ] 認証状態がページリフレッシュ後も維持される
- [ ] すべてのセキュリティベストプラクティスに従っている

## 依存関係

### バックエンドライブラリ

```lisp
;; dogatto.asdに追加
:ironclad       ; 暗号化（パスワードハッシュ化）
:babel          ; 文字エンコーディング
:cl-redis       ; Redisクライアント（既に含まれている）
:local-time     ; 時間処理
```

### フロントエンドライブラリ

```bash
npm install react-router-dom@6  # ルーティング
```

## ロールアウト計画

1. **フェーズ1：バックエンドAPI**（1〜2日目）
   - パスワードハッシュ化ユーティリティの実装
   - 認証コントローラーの作成
   - 登録エンドポイントの実装
   - ログインエンドポイントの実装
   - ログアウトエンドポイントの実装
   - 現在のユーザーエンドポイントの実装

2. **フェーズ2：セッション管理**（3日目）
   - Redisセッションストレージ
   - セッション作成/検証
   - 認証ミドルウェア

3. **フェーズ3：フロントエンド**（4〜5日目）
   - ログインページ
   - 登録ページ
   - ProtectedRouteコンポーネント
   - AuthContextの統合

4. **フェーズ4：統合**（6日目）
   - フロントエンドとバックエンドの接続
   - エラーハンドリング
   - テスト

5. **フェーズ5：仕上げ**（7日目）
   - UI改善
   - エラーメッセージ
   - ドキュメント
   - セキュリティレビュー

## 参考資料

- bcrypt: https://en.wikipedia.org/wiki/Bcrypt
- OWASPパスワード保存: https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html
- セッション管理: https://cheatsheetseries.owasp.org/cheatsheets/Session_Management_Cheat_Sheet.html
