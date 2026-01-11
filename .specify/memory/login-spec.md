# ログイン画面仕様

## 概要

ユーザーがDOGATTOシステムにログインするための画面。メールアドレス/パスワード認証とGitHub OAuth認証の2つの方法をサポートする。

## URL

- エンドポイント: `/login`
- メソッド: GET (画面表示), POST (認証処理)

## 機能要件

### 1. ログインフォーム

#### 1.1 メールアドレス/パスワードログイン

**入力項目:**
- メールアドレス (email)
  - 必須項目
  - 形式: RFC 5322準拠のメールアドレス
  - 最大長: 255文字
  - バリデーション: メールアドレス形式チェック
  
- パスワード (password)
  - 必須項目
  - 入力タイプ: password (マスク表示)
  - 最小長: 8文字
  - 最大長: 72文字
  - バリデーション: 最小長チェック

**ボタン:**
- ログインボタン
  - テキスト: "ログイン" / "Login"
  - アクション: フォーム送信 (POST `/api/auth/login`)
  - 無効化条件: 入力項目が未入力またはバリデーションエラー時

#### 1.2 GitHub OAuth ログイン

**ボタン:**
- GitHubでログインボタン
  - テキスト: "GitHubでログイン" / "Login with GitHub"
  - アイコン: GitHubロゴ
  - アクション: OAuth認証フロー開始 (GET `/api/auth/github`)
  - 新しいウィンドウまたはリダイレクト

### 2. ナビゲーション

#### 2.1 ユーザー登録画面へのリンク

- テキスト: "アカウントをお持ちでない方はこちら" / "Don't have an account? Sign up"
- リンク先: `/signup`
- 配置: フォーム下部

#### 2.2 パスワードリセットへのリンク (将来実装)

- テキスト: "パスワードをお忘れの方" / "Forgot password?"
- リンク先: `/password-reset`
- 配置: パスワード入力欄の下

## API仕様

### 3.1 メールアドレス/パスワードログイン

**エンドポイント:** `POST /api/auth/login`

**リクエスト:**
```json
{
  "email": "user@example.com",
  "password": "password123"
}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "user": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "email": "user@example.com",
      "name": "User Name",
      "created_at": "2026-01-10T00:00:00Z"
    },
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "expires_in": 86400
  }
}
```

**レスポンス (失敗):**
```json
{
  "status": "error",
  "error": {
    "code": "AUTH_FAILED",
    "message": "メールアドレスまたはパスワードが正しくありません"
  }
}
```

**ステータスコード:**
- 200: ログイン成功
- 400: バリデーションエラー
- 401: 認証失敗
- 429: レート制限超過
- 500: サーバーエラー

### 3.2 GitHub OAuth ログイン

**エンドポイント:** `GET /api/auth/github`

**フロー:**
1. ユーザーがボタンをクリック
2. GitHub OAuth認証ページにリダイレクト
3. ユーザーが認証を許可
4. コールバックURL (`/api/auth/github/callback`) にリダイレクト
5. バックエンドでトークン取得・ユーザー情報取得
6. JWTトークン発行
7. フロントエンドにリダイレクト (トークンをクエリパラメータまたはCookieで渡す)

**コールバックエンドポイント:** `GET /api/auth/github/callback`

**クエリパラメータ:**
- `code`: GitHub認証コード
- `state`: CSRF対策用のstate

**レスポンス:**
- 成功時: フロントエンドにリダイレクト (`/dashboard?token=...`)
- 失敗時: ログイン画面にリダイレクト (`/login?error=...`)

## セキュリティ要件

### 4.1 CSRF対策

- フォーム送信時にCSRFトークンを含める
- GitHub OAuth認証時にstateパラメータを使用

### 4.2 レート制限

- IPアドレスごとに5回/分のログイン試行制限
- 制限超過時は429エラーを返す
- 一定時間後に自動解除

### 4.3 パスワード

- パスワードはbcryptでハッシュ化して保存
- 平文パスワードはログに記録しない
- HTTPS通信必須

### 4.4 セッション管理

- JWTトークンを使用
- トークン有効期限: 24時間
- リフレッシュトークン: 将来実装

## UI/UX要件

### 5.1 レイアウト

- 中央配置のログインフォーム
- レスポンシブデザイン (モバイル対応)
- アクセシビリティ対応 (ARIA属性、キーボード操作)

### 5.2 バリデーション

- リアルタイムバリデーション (入力中)
- エラーメッセージは入力欄の下に表示
- 日本語と英語の両方をサポート

### 5.3 ローディング状態

- ログインボタン押下時にローディングインジケーター表示
- 二重送信防止

### 5.4 エラー表示

- 認証失敗時はフォーム上部にエラーメッセージを表示
- エラーメッセージ例:
  - "メールアドレスまたはパスワードが正しくありません"
  - "アカウントがロックされています。しばらくしてから再試行してください"
  - "ネットワークエラーが発生しました"

## データモデル

### 6.1 Usersテーブル

```sql
CREATE TABLE users (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  email VARCHAR(255) UNIQUE NOT NULL,
  password_hash VARCHAR(255),  -- NULL可 (OAuth専用ユーザー)
  name VARCHAR(255),
  provider VARCHAR(50),  -- 'local' or 'github'
  provider_id VARCHAR(255),  -- GitHubのユーザーID
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  INDEX idx_ulid (ulid),
  INDEX idx_email (email)
);
```

### 6.2 Login_Attemptsテーブル (レート制限用)

```sql
CREATE TABLE login_attempts (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ip_address VARCHAR(45) NOT NULL,
  email VARCHAR(255),
  attempted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  success BOOLEAN DEFAULT FALSE,
  INDEX idx_ip_time (ip_address, attempted_at)
);
```

## 実装順序

### フェーズ1: 基本機能
1. ログイン画面UI作成
2. メールアドレス/パスワードログインAPI実装
3. JWTトークン発行・検証機能
4. バリデーション実装

### フェーズ2: OAuth認証
1. GitHub OAuth設定
2. OAuth認証フロー実装
3. コールバック処理実装

### フェーズ3: セキュリティ強化
1. レート制限実装
2. CSRF対策
3. セキュリティヘッダー設定

## テスト要件

### 7.1 単体テスト
- バリデーション関数のテスト
- パスワードハッシュ化のテスト
- JWTトークン生成・検証のテスト

### 7.2 統合テスト
- ログイン成功シナリオ
- ログイン失敗シナリオ
- OAuth認証フロー
- レート制限の動作確認

### 7.3 E2Eテスト
- ブラウザでのログインフロー
- モバイルでのログインフロー
- エラーハンドリング

## 非機能要件

### 8.1 パフォーマンス
- ログインAPI応答時間: 200ms以内
- ページロード時間: 1秒以内

### 8.2 可用性
- システム稼働率: 99.9%以上

### 8.3 スケーラビリティ
- 同時ログイン: 1000ユーザー対応

## 将来の拡張

- パスワードリセット機能
- 2要素認証 (2FA)
- Google/Twitter OAuth対応
- リフレッシュトークン
- ログイン履歴の表示
- デバイス管理機能

---

**Version**: 1.0.0
**Created**: 2026-01-11
**Last Updated**: 2026-01-11
