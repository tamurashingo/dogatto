# ホーム画面仕様

## 概要

DOGATTOシステムのメイン画面。ユーザーの認証状態に応じて表示内容を切り替える。認証済みユーザーにはTODO管理機能を提供し、未認証ユーザーにはプロダクト紹介を表示する。

## URL

- エンドポイント: `/`
- メソッド: GET

## 表示内容の分岐

### 1. セッション情報なし（未ログイン）

**表示内容:**
- プロダクト紹介画面（ランディングページ）
- GitHub.comのトップページのようなデザイン

**プロダクト紹介画面の構成:**
- ヒーローセクション
  - キャッチコピー
  - サブタイトル
  - CTA（Call To Action）ボタン
    - "今すぐ始める" → `/signup`
    - "ログイン" → `/login`
  
- 機能紹介セクション
  - 複数タグによる柔軟な分類
  - ラベルによるタグのグルーピング
  - TODO共有機能
  - マルチプラットフォーム対応
  
- フッター
  - リンク（利用規約、プライバシーポリシーなど）
  - GitHub リポジトリへのリンク
  - コピーライト

### 2. セッション情報あり（タイムアウト）

**判定条件:**
- JWTトークンの有効期限切れ
- またはサーバー側でのセッション無効化

**動作:**
- ログイン画面 (`/login`) へ自動リダイレクト
- リダイレクト時にメッセージを表示
  - "セッションがタイムアウトしました。再度ログインしてください。"
- トークンをクリア（localStorage/Cookie）

### 3. セッション情報あり（仮登録状態）

**判定条件:**
- ユーザーステータスが `pending_verification`

**動作:**
- 本登録待ち画面 (`/verify-pending`) へ自動リダイレクト

### 4. セッション情報あり（本登録完了）

**判定条件:**
- ユーザーステータスが `active`
- 有効なJWTトークンを保持

**表示内容:**
- ホーム画面（TODO管理画面）

## ホーム画面の構成

### レイアウト

```
+--------------------------------------------------+
| ヘッダー                                          |
| [DOGATTO] [検索] [タグ] [ラベル] [ユーザーメニュー] |
+--------------------------------------------------+
| サイドバー          | メインコンテンツエリア        |
| - ラベル一覧        | +------------------------+  |
| - クイックフィルター | | TODO検索条件エリア      |  |
|                    | +------------------------+  |
|                    | +------------------------+  |
|                    | | 表示ラベル名            |  |
|                    | +------------------------+  |
|                    | +------------------------+  |
|                    | | TODO一覧                |  |
|                    | | - TODO項目1             |  |
|                    | | - TODO項目2             |  |
|                    | | ...                    |  |
|                    | +------------------------+  |
|                    | +------------------------+  |
|                    | | TODO登録エリア          |  |
|                    | +------------------------+  |
+--------------------------------------------------+
```

### 4.1 ヘッダー

**要素:**
- ロゴ: DOGATTO
- グローバル検索バー
  - プレースホルダー: "TODOを検索..."
  - 全文検索機能
- タグ管理ボタン
  - テキスト: "タグ"
  - リンク先: `/tags`
- ラベル管理ボタン
  - テキスト: "ラベル"
  - リンク先: `/labels`
- ユーザーメニュー（ドロップダウン）
  - ユーザー名表示
  - プロフィール → `/profile`
  - 設定 → `/settings`
  - ログアウト

### 4.2 サイドバー

**ラベル一覧:**
- 登録済みラベルの一覧表示
- クリックで該当ラベルのTODOをフィルタリング
- ラベル追加ボタン
- 各ラベルの表示:
  - ラベル名
  - 所属TODO数のバッジ

**クイックフィルター:**
- すべて
- 今日
- 今週
- 期限切れ
- 完了済み
- 未完了

### 4.3 TODO検索条件エリア

**検索フィルター:**
- キーワード検索
  - プレースホルダー: "TODOを検索..."
  
- タグフィルター
  - タグの複数選択（チェックボックスまたはタグクリック）
  - AND/OR検索の切り替え
  - 選択中のタグをバッジ表示
  - クリアボタン
  
- 日付フィルター
  - 期限日の範囲指定
  - クイック選択（今日、今週、今月など）
  
- ステータスフィルター
  - 未完了
  - 完了
  - すべて
  
- ソート
  - 作成日（新しい順/古い順）
  - 期限日（近い順/遠い順）
  - 更新日（新しい順/古い順）
  - タイトル（昇順/降順）

### 4.4 表示ラベル名エリア

**表示内容:**
- 現在表示中のラベル名
  - 例: "DAILY" / "WORKOUT" / "すべて"
- ラベルの説明（ある場合）
- 該当TODO数
  - 例: "15件のTODO"

**ラベル選択方法:**
- サイドバーのラベル一覧からクリック
- デフォルト: "すべて"または最初に登録したラベル

### 4.5 TODO一覧エリア

**表示形式:**
- リスト形式
- カード形式（切り替え可能）

**TODO項目の表示内容:**
- チェックボックス（完了/未完了の切り替え）
- TODOタイトル
- タグ一覧（バッジ表示）
- 期限日（ある場合）
- 共有状態アイコン（共有されている場合）
- 編集ボタン
- 削除ボタン

**インタラクション:**
- チェックボックスクリック: TODO完了/未完了の切り替え
- タイトルクリック: TODO詳細画面へ遷移
- タグクリック: 該当タグでフィルタリング
- 編集ボタン: TODO編集モーダル表示
- 削除ボタン: 削除確認ダイアログ表示

**ページネーション:**
- 1ページあたり20件表示
- ページャー表示（前へ/次へ、ページ番号）
- または無限スクロール（オプション）

### 4.6 TODO登録エリア

**表示位置:**
- TODO一覧の下部
- またはフローティングボタン（+ ボタン）

**入力項目（インラインフォーム）:**
- TODOタイトル
  - プレースホルダー: "新しいTODOを入力..."
  - 最大長: 255文字
  
- タグ追加
  - 既存タグから選択
  - 新規タグの作成
  - タグの複数選択
  
- 期限日（オプション）
  - 日付ピッカー
  
- 追加ボタン
  - テキスト: "追加" / "Add"

**クイック追加モード:**
- タイトルのみ入力してEnterで即座に追加
- 詳細設定は後で編集可能

**詳細登録モード:**
- "詳細を設定"ボタンクリックでモーダル表示
- 詳細項目:
  - タイトル（必須）
  - 説明文（任意）
  - タグ（複数選択可）
  - 期限日（任意）
  - 優先度（任意）

## API仕様

### 5.1 ユーザー情報取得

**エンドポイント:** `GET /api/user`

**ヘッダー:**
```
Authorization: Bearer {jwt_token}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "user": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "email": "user@example.com",
      "username": "user@example.com",
      "status": "active",
      "created_at": "2026-01-10T00:00:00Z"
    }
  }
}
```

**レスポンス (セッションタイムアウト):**
```json
{
  "status": "error",
  "error": {
    "code": "UNAUTHORIZED",
    "message": "セッションがタイムアウトしました"
  }
}
```

**ステータスコード:**
- 200: 成功
- 401: 認証エラー（トークン無効/期限切れ）
- 403: アクセス禁止（仮登録状態など）

### 5.2 TODO一覧取得

**エンドポイント:** `GET /api/todos`

**クエリパラメータ:**
- `label_ulid`: ラベルのULID（オプション）
- `tag_ulids`: タグのULID（カンマ区切り、オプション）
- `tag_match`: タグの一致条件 (`and` または `or`、デフォルト: `and`)
- `keyword`: 検索キーワード（オプション）
- `status`: ステータス (`active`, `completed`, `all`、デフォルト: `active`)
- `due_date_from`: 期限日の開始（ISO 8601形式、オプション）
- `due_date_to`: 期限日の終了（ISO 8601形式、オプション）
- `sort`: ソート順 (`created_asc`, `created_desc`, `due_asc`, `due_desc`, `updated_asc`, `updated_desc`, `title_asc`, `title_desc`、デフォルト: `created_desc`)
- `page`: ページ番号（デフォルト: 1）
- `per_page`: 1ページあたりの件数（デフォルト: 20、最大: 100）

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "todos": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "title": "体重測定",
        "description": "毎朝の体重測定",
        "status": "active",
        "due_date": "2026-01-12",
        "tags": [
          {
            "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
            "name": "DAILY"
          }
        ],
        "is_shared": false,
        "created_at": "2026-01-10T00:00:00Z",
        "updated_at": "2026-01-10T00:00:00Z"
      }
    ],
    "pagination": {
      "current_page": 1,
      "total_pages": 5,
      "total_count": 95,
      "per_page": 20
    }
  }
}
```

### 5.3 TODO作成

**エンドポイント:** `POST /api/todos`

**リクエスト:**
```json
{
  "title": "新しいTODO",
  "description": "説明文（オプション）",
  "tag_ulids": ["01ARZ3NDEKTSV4RRFFQ69G5FAV"],
  "due_date": "2026-01-15"
}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "todo": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "title": "新しいTODO",
      "description": "説明文（オプション）",
      "status": "active",
      "due_date": "2026-01-15",
      "tags": [
        {
          "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
          "name": "DAILY"
        }
      ],
      "is_shared": false,
      "created_at": "2026-01-11T00:00:00Z",
      "updated_at": "2026-01-11T00:00:00Z"
    }
  }
}
```

### 5.4 TODO完了/未完了切り替え

**エンドポイント:** `PATCH /api/todos/{ulid}/toggle`

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "todo": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "status": "completed",
      "completed_at": "2026-01-11T00:00:00Z"
    }
  }
}
```

### 5.5 ラベル一覧取得

**エンドポイント:** `GET /api/labels`

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "labels": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "DAILY",
        "description": "日々のタスク",
        "todo_count": 15,
        "tags": [
          {
            "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
            "name": "DAILY"
          }
        ],
        "created_at": "2026-01-10T00:00:00Z"
      }
    ]
  }
}
```

## セキュリティ要件

### 6.1 認証・認可

- すべてのAPI呼び出しにJWTトークンが必要
- トークンの有効期限チェック
- ユーザーステータスのチェック（`active` のみアクセス可能）

### 6.2 CSRF対策

- 状態変更のAPI（POST/PATCH/DELETE）にはCSRFトークンを要求

### 6.3 XSS対策

- ユーザー入力のサニタイズ
- HTMLエスケープ処理

## UI/UX要件

### 7.1 レスポンシブデザイン

**デスクトップ（1024px以上）:**
- サイドバー + メインコンテンツの2カラムレイアウト

**タブレット（768px - 1023px）:**
- サイドバーは折りたたみ可能
- ハンバーガーメニューで表示

**モバイル（767px以下）:**
- 1カラムレイアウト
- サイドバーはドロワーメニュー
- TODO登録はフローティングボタン

### 7.2 アクセシビリティ

- キーボード操作対応
- スクリーンリーダー対応
- ARIA属性の適切な使用
- 十分なコントラスト比

### 7.3 パフォーマンス

- 初回ロード時間: 2秒以内
- API応答後のレンダリング: 100ms以内
- スムーズなスクロール
- 楽観的UI更新（Optimistic UI）

### 7.4 ローディング状態

- 初回ロード時: スケルトンスクリーン表示
- TODO作成時: ローディングインジケーター
- TODO切り替え時: 即座にUIを更新（楽観的更新）

### 7.5 エラーハンドリング

- ネットワークエラー: トースト通知で表示
- 認証エラー: ログイン画面へリダイレクト
- バリデーションエラー: フォーム内にエラーメッセージ表示

## データモデル

### 8.1 TODOsテーブル

```sql
CREATE TABLE todos (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  user_id BIGINT NOT NULL,
  title VARCHAR(255) NOT NULL,
  description TEXT,
  status ENUM('active', 'completed', 'archived') DEFAULT 'active',
  due_date DATE,
  completed_at TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_status (status),
  INDEX idx_due_date (due_date),
  INDEX idx_created_at (created_at)
);
```

### 8.2 Tagsテーブル

```sql
CREATE TABLE tags (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  user_id BIGINT NOT NULL,
  name VARCHAR(100) NOT NULL,
  color VARCHAR(7),  -- HEX color code
  merged_to_tag_id BIGINT NULL,  -- マージ先のタグID
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (merged_to_tag_id) REFERENCES tags(id) ON DELETE SET NULL,
  UNIQUE KEY uk_user_name (user_id, name),
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_merged_to (merged_to_tag_id)
);
```

### 8.3 TODO_Tagsテーブル（中間テーブル）

```sql
CREATE TABLE todo_tags (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  todo_id BIGINT NOT NULL,
  tag_id BIGINT NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (todo_id) REFERENCES todos(id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE,
  UNIQUE KEY uk_todo_tag (todo_id, tag_id),
  INDEX idx_todo_id (todo_id),
  INDEX idx_tag_id (tag_id)
);
```

### 8.4 Labelsテーブル

```sql
CREATE TABLE labels (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  user_id BIGINT NOT NULL,
  name VARCHAR(100) NOT NULL,
  description TEXT,
  merged_to_label_id BIGINT NULL,  -- マージ先のラベルID
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (merged_to_label_id) REFERENCES labels(id) ON DELETE SET NULL,
  UNIQUE KEY uk_user_name (user_id, name),
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_merged_to (merged_to_label_id)
);
```

### 8.5 Label_Tagsテーブル（中間テーブル）

```sql
CREATE TABLE label_tags (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  label_id BIGINT NOT NULL,
  tag_id BIGINT NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE,
  UNIQUE KEY uk_label_tag (label_id, tag_id),
  INDEX idx_label_id (label_id),
  INDEX idx_tag_id (tag_id)
);
```

## 実装順序

### フェーズ1: 基本画面
1. プロダクト紹介画面の作成
2. 認証状態の判定ロジック
3. ホーム画面の基本レイアウト

### フェーズ2: TODO基本機能
1. TODO一覧表示
2. TODO作成（クイック追加）
3. TODO完了/未完了切り替え
4. TODO削除

### フェーズ3: フィルタリング・検索
1. ラベルによるフィルタリング
2. タグによるフィルタリング
3. キーワード検索
4. 日付フィルター
5. ソート機能

### フェーズ4: UI強化
1. TODO詳細登録モーダル
2. レスポンシブデザイン対応
3. ページネーション
4. ローディング・エラー表示

## テスト要件

### 9.1 単体テスト
- 認証状態判定ロジック
- フィルタリングロジック
- ソートロジック
- バリデーション

### 9.2 統合テスト
- ログイン状態でのアクセス
- 未ログイン状態でのアクセス
- セッションタイムアウト時の動作
- TODO CRUD操作
- フィルタリング・検索機能

### 9.3 E2Eテスト
- プロダクト紹介からサインアップ
- ログインからホーム画面表示
- TODO作成から完了まで
- フィルタリング操作
- レスポンシブデザインの確認

## 非機能要件

### 10.1 パフォーマンス
- 初回ロード: 2秒以内
- TODO一覧取得API: 200ms以内
- TODO作成API: 100ms以内
- UI更新: 60fps維持

### 10.2 可用性
- システム稼働率: 99.9%以上

### 10.3 スケーラビリティ
- 1ユーザーあたり10,000 TODO対応
- 100同時接続ユーザー対応

## 将来の拡張

- カンバンビュー
- カレンダービュー
- TODO詳細画面
- TODO編集履歴
- コメント機能
- 添付ファイル機能
- リマインダー機能
- TODO共有管理画面
- ダークモード対応
- キーボードショートカット
- ドラッグ&ドロップでの並び替え
- バルク操作（一括削除、一括完了など）

## 関連仕様

- ログイン画面仕様: `login-spec.md`
- ユーザー登録画面仕様: `signup-spec.md`
- タグ管理画面仕様: `tags-spec.md` (今後作成)
- ラベル管理画面仕様: `labels-spec.md` (今後作成)
- TODO詳細画面仕様: `todo-detail-spec.md` (今後作成)

---

**Version**: 1.0.0
**Created**: 2026-01-11
**Last Updated**: 2026-01-11
