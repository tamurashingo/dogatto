# ユーザー登録画面仕様

## 概要

新規ユーザーがDOGATTOシステムに登録するための画面。メールアドレス/パスワード登録とGitHub OAuth登録の2つの方法をサポートする。登録後はメール認証による本登録が必要。

## URL

- エンドポイント: `/signup`
- メソッド: GET (画面表示), POST (登録処理)

## 機能要件

### 1. ユーザー登録フォーム

#### 1.1 メールアドレス/パスワード登録

**入力項目:**
- メールアドレス (email)
  - 必須項目
  - 形式: RFC 5322準拠のメールアドレス
  - 最大長: 255文字
  - バリデーション: メールアドレス形式チェック
  - 重複チェック: 既存ユーザーとの重複を確認
  
- パスワード (password)
  - 必須項目
  - 入力タイプ: password (マスク表示)
  - 最小長: 8文字
  - 最大長: 72文字
  - 要件: 英数字を含むこと（推奨）
  - バリデーション: 最小長チェック

- パスワード確認 (password_confirmation)
  - 必須項目
  - 入力タイプ: password (マスク表示)
  - バリデーション: パスワードと一致すること

**自動生成項目:**
- ユーザー名 (username)
  - 初期値: メールアドレス（そのまま）
  - 例: `taro@example.com` → `taro@example.com`
  - システム内でユニークである必要がある
  - ユーザーは後で変更可能（プロフィール画面で）
  - 変更時はユニーク制約をチェック

**ボタン:**
- 登録ボタン
  - テキスト: "登録" / "Sign up"
  - アクション: フォーム送信 (POST `/api/auth/signup`)
  - 無効化条件: 入力項目が未入力またはバリデーションエラー時

#### 1.2 GitHub OAuth 登録

**ボタン:**
- GitHubで登録ボタン
  - テキスト: "GitHubで登録" / "Sign up with GitHub"
  - アイコン: GitHubロゴ
  - アクション: OAuth認証フロー開始 (GET `/api/auth/github/signup`)
  - 新しいウィンドウまたはリダイレクト

**GitHub連携で取得する情報:**
- メールアドレス
- GitHubアカウント名（ユーザー名として使用）

**自動生成項目:**
- ユーザー名 (username)
  - 初期値: メールアドレス（そのまま）
  - 例: GitHubメールアドレス `taro@example.com` → ユーザー名 `taro@example.com`
  - システム内でユニークである必要がある
  - ユーザーは後で変更可能（プロフィール画面で）

### 2. ナビゲーション

#### 2.1 ログイン画面へのリンク

- テキスト: "すでにアカウントをお持ちの方はこちら" / "Already have an account? Login"
- リンク先: `/login`
- 配置: フォーム下部

## API仕様

### 3.1 メールアドレス/パスワード登録

**エンドポイント:** `POST /api/auth/signup`

**リクエスト:**
```json
{
  "email": "taro@example.com",
  "password": "password123",
  "password_confirmation": "password123"
}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "user": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "email": "taro@example.com",
      "username": "taro@example.com",
      "status": "pending_verification",
      "created_at": "2026-01-11T00:00:00Z"
    },
    "message": "登録確認メールを送信しました。メールを確認して登録を完了してください。"
  }
}
```

**レスポンス (失敗 - メールアドレス重複):**
```json
{
  "status": "error",
  "error": {
    "code": "EMAIL_ALREADY_EXISTS",
    "message": "このメールアドレスは既に登録されています"
  }
}
```

**レスポンス (失敗 - バリデーションエラー):**
```json
{
  "status": "error",
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "入力内容に誤りがあります",
    "details": {
      "email": ["有効なメールアドレスを入力してください"],
      "password": ["パスワードは8文字以上で入力してください"],
      "password_confirmation": ["パスワードが一致しません"]
    }
  }
}
```

**ステータスコード:**
- 201: 登録成功（仮登録）
- 400: バリデーションエラー
- 409: メールアドレス重複
- 429: レート制限超過
- 500: サーバーエラー

### 3.2 GitHub OAuth 登録

**エンドポイント:** `GET /api/auth/github/signup`

**フロー:**
1. ユーザーがボタンをクリック
2. GitHub OAuth認証ページにリダイレクト
3. ユーザーが認証を許可
4. コールバックURL (`/api/auth/github/signup/callback`) にリダイレクト
5. バックエンドでトークン取得・ユーザー情報取得（メールアドレス、アカウント名のみ）
6. メールアドレスの重複チェック
7. ユーザー登録（仮登録状態）
8. 確認メール送信
9. フロントエンドにリダイレクト

**コールバックエンドポイント:** `GET /api/auth/github/signup/callback`

**クエリパラメータ:**
- `code`: GitHub認証コード
- `state`: CSRF対策用のstate

**レスポンス:**
- 成功時: 登録完了画面にリダイレクト (`/signup/complete`)
- 失敗時: 登録画面にリダイレクト (`/signup?error=...`)

### 3.3 メール認証

**エンドポイント:** `GET /api/auth/verify-email`

**クエリパラメータ:**
- `token`: メール認証トークン（ULID + ランダム文字列）
- 有効期限: 24時間

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "user": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "email": "taro@example.com",
      "username": "taro@example.com",
      "status": "active",
      "verified_at": "2026-01-11T00:30:00Z"
    },
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "expires_in": 86400
  }
}
```

**フロー:**
1. ユーザーがメール内のリンクをクリック
2. バックエンドでトークン検証
3. ユーザーステータスを `active` に更新
4. JWTトークン発行
5. ホーム画面にリダイレクト (`/?token=...`)

**レスポンス (失敗):**
- トークン無効: `/signup/verify-error?reason=invalid_token`
- トークン期限切れ: `/signup/verify-error?reason=expired_token`

### 3.4 確認メール再送信

**エンドポイント:** `POST /api/auth/resend-verification`

**リクエスト:**
```json
{
  "email": "taro@example.com"
}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "message": "確認メールを再送信しました"
}
```

## セキュリティ要件

### 4.1 CSRF対策

- フォーム送信時にCSRFトークンを含める
- GitHub OAuth認証時にstateパラメータを使用

### 4.2 レート制限

- IPアドレスごとに3回/時間の登録試行制限
- 制限超過時は429エラーを返す
- 確認メール再送信: 1回/5分

### 4.3 パスワード

- パスワードはbcryptでハッシュ化して保存（cost factor: 12）
- 平文パスワードはログに記録しない
- HTTPS通信必須

### 4.4 メールアドレス重複チェック

- 登録済みメールアドレスは登録不可
- 過去に登録があったメールアドレスも登録不可
- Gmailのエイリアス（+記号、ドット）は考慮しない
  - `taro@gmail.com`、`taro+1@gmail.com`、`ta.ro@gmail.com` は別アドレスとして扱う

### 4.5 メール認証トークン

- トークン形式: ULID + 32文字のランダム文字列
- 有効期限: 24時間
- 1回のみ使用可能（使用後は無効化）
- データベースに暗号化して保存

## ユーザーステータス管理

### 5.1 ステータス定義

- `pending_verification`: 仮登録状態（メール未認証）
- `active`: 本登録状態（メール認証済み）
- `suspended`: アカウント停止
- `deleted`: 削除済み

### 5.2 仮登録状態の制約

**仮登録状態でログインした場合:**
- ログインは可能だがホーム画面には遷移できない
- 本登録待ち画面 (`/verify-pending`) に自動リダイレクト
- 本登録待ち画面の内容:
  - メール認証が必要な旨を表示
  - 確認メール再送信ボタン
  - ログアウトボタン

**API アクセス制限:**
- 仮登録状態ではTODO/TAG/LABEL関連のAPIにアクセス不可
- ユーザー情報取得、確認メール再送信のみ可能

## UI/UX要件

### 6.1 レイアウト

- 中央配置の登録フォーム
- レスポンシブデザイン (モバイル対応)
- アクセシビリティ対応 (ARIA属性、キーボード操作)

### 6.2 バリデーション

- リアルタイムバリデーション (入力中)
- エラーメッセージは入力欄の下に表示
- パスワード強度インジケーター表示
- 日本語と英語の両方をサポート

### 6.3 ローディング状態

- 登録ボタン押下時にローディングインジケーター表示
- 二重送信防止

### 6.4 成功時の表示

- 登録完了画面 (`/signup/complete`) に遷移
- 表示内容:
  - 登録完了メッセージ
  - メール確認を促すメッセージ
  - 登録したメールアドレスの表示
  - 確認メール再送信リンク

### 6.5 エラー表示

- エラーメッセージ例:
  - "このメールアドレスは既に登録されています"
  - "パスワードは8文字以上で入力してください"
  - "パスワードが一致しません"
  - "ネットワークエラーが発生しました"

## データモデル

### 7.1 Usersテーブル（拡張）

```sql
CREATE TABLE users (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  email VARCHAR(255) UNIQUE NOT NULL,
  username VARCHAR(255) UNIQUE NOT NULL,  -- システム内でユニーク
  password_hash VARCHAR(255),  -- NULL可 (OAuth専用ユーザー)
  provider VARCHAR(50) DEFAULT 'local',  -- 'local' or 'github'
  provider_id VARCHAR(255),  -- GitHubのユーザーID
  status ENUM('pending_verification', 'active', 'suspended', 'deleted') DEFAULT 'pending_verification',
  verified_at TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,  -- 論理削除用
  INDEX idx_ulid (ulid),
  INDEX idx_email (email),
  INDEX idx_username (username),
  INDEX idx_status (status)
);
```

### 7.2 Email_Verification_Tokensテーブル

```sql
CREATE TABLE email_verification_tokens (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  user_id BIGINT NOT NULL,
  token_hash VARCHAR(255) UNIQUE NOT NULL,  -- トークンのハッシュ値
  expires_at TIMESTAMP NOT NULL,
  used_at TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  INDEX idx_token_hash (token_hash),
  INDEX idx_expires_at (expires_at)
);
```

### 7.3 Signup_Attemptsテーブル (レート制限用)

```sql
CREATE TABLE signup_attempts (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ip_address VARCHAR(45) NOT NULL,
  email VARCHAR(255),
  attempted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  success BOOLEAN DEFAULT FALSE,
  INDEX idx_ip_time (ip_address, attempted_at)
);
```

## メール仕様

### 8.1 確認メール

**件名:** 【DOGATTO】メールアドレスの確認

**本文:**
```
{username} 様

DOGATTOへのご登録ありがとうございます。

以下のリンクをクリックして、メールアドレスの確認を完了してください。

{verification_url}

このリンクの有効期限は24時間です。

※このメールに心当たりがない場合は、このメールを無視してください。

---
DOGATTO
```

**verification_url 例:**
```
https://dogatto.example.com/api/auth/verify-email?token=01ARZ3NDEKTSV4RRFFQ69G5FAV1a2b3c4d5e6f7g8h9i0j1k2l3m4n5o6
```

## 実装順序

### フェーズ1: 基本機能
1. 登録画面UI作成
2. メールアドレス/パスワード登録API実装
3. ユーザー名自動生成機能
4. バリデーション実装

### フェーズ2: メール認証
1. メール送信機能実装
2. メール認証トークン生成・検証機能
3. 本登録待ち画面実装
4. 確認メール再送信機能

### フェーズ3: OAuth認証
1. GitHub OAuth登録フロー実装
2. コールバック処理実装

### フェーズ4: セキュリティ強化
1. レート制限実装
2. CSRF対策
3. メールアドレス重複チェックの強化

## テスト要件

### 9.1 単体テスト
- バリデーション関数のテスト
- パスワードハッシュ化のテスト
- ユーザー名生成ロジックのテスト（メールアドレスをそのまま使用）
- ユーザー名ユニーク制約のテスト
- トークン生成・検証のテスト

### 9.2 統合テスト
- 登録成功シナリオ
- メールアドレス重複エラー
- メール認証フロー
- OAuth登録フロー
- レート制限の動作確認
- 仮登録状態でのアクセス制限

### 9.3 E2Eテスト
- ブラウザでの登録フロー
- メール認証フロー
- モバイルでの登録フロー
- エラーハンドリング

## 非機能要件

### 10.1 パフォーマンス
- 登録API応答時間: 200ms以内（メール送信を除く）
- メール送信: 非同期処理（キュー使用）
- ページロード時間: 1秒以内

### 10.2 可用性
- システム稼働率: 99.9%以上
- メール送信失敗時のリトライ機構

### 10.3 スケーラビリティ
- 同時登録: 100ユーザー対応

## 将来の拡張

- ユーザー名変更機能（プロフィール画面で実装）
- メールアドレス変更機能
- 電話番号認証
- SMS認証
- Google/Twitter OAuth対応
- パスワード強度チェックの強化
- Gmailエイリアスの正規化機能（オプション）
- アカウント削除機能
- 削除アカウントの一定期間後の再登録許可

## 関連仕様

- ログイン画面仕様: `login-spec.md`
- ホーム画面仕様: `home-spec.md` (今後作成)
- プロフィール画面仕様: (今後作成)

---

**Version**: 1.1.0
**Created**: 2026-01-11
**Last Updated**: 2026-01-11
