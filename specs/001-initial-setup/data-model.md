# データベーススキーマ設計

## 概要

DOGATTOシステムのデータベーススキーマ定義。ユーザー、TODO、タグ、ラベル、コメントの管理を行う。マルチテナント対応でユーザーごとにデータを分離する。

## 設計原則

### 1. マルチテナント対応

- すべての主要テーブルに `user_id` を持つ
- ユーザーは自分のデータのみアクセス可能
- 外部キー制約で CASCADE DELETE を設定

### 2. ULID による外部アクセス

- すべてのテーブルに `ulid` を持つ
- 外部からのアクセスは ULID ベース
- 内部的には `id` (auto increment) を使用

### 3. マージ機能のサポート

- タグとラベルはマージ可能
- `merged_to_*` フィールドでマージチェーンを管理

### 4. 論理削除

- `deleted_at` フィールドで論理削除
- 物理削除は行わない（マージの場合を除く）

### 5. タイムスタンプ

- `created_at`: 作成日時
- `updated_at`: 更新日時
- `deleted_at`: 削除日時（論理削除）

## テーブル一覧

1. **users** - ユーザー情報
2. **todos** - TODO
3. **todo_comments** - TODO コメント
4. **tags** - タグ
5. **todo_tags** - TODO-タグ ジャンクションテーブル
6. **labels** - ラベル
7. **label_tags** - ラベル-タグ ジャンクションテーブル
8. **email_verification_tokens** - メール認証トークン
9. **login_attempts** - ログイン試行履歴
10. **signup_attempts** - サインアップ試行履歴

## ER図

```
users
  │
  ├─→ todos ←─→ todo_tags ←─→ tags ←─→ label_tags ←─┐
  │     │                       │                     │
  │     └─→ todo_comments       │                     │
  │                              │                     │
  └─→ tags ──────────────────────┘                     │
  │                                                    │
  └─→ labels ←────────────────────────────────────────┘
```

## テーブル定義

### 1. users - ユーザー情報

```sql
CREATE TABLE users (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  
  -- ユーザー情報
  email VARCHAR(255) UNIQUE NOT NULL,
  username VARCHAR(255) UNIQUE NOT NULL,
  password_hash VARCHAR(255),  -- NULL可（OAuth専用ユーザー）
  
  -- OAuth情報
  provider VARCHAR(50) DEFAULT 'local',  -- 'local', 'github'
  provider_id VARCHAR(255),              -- OAuth Provider のユーザーID
  
  -- ステータス
  status ENUM('pending_verification', 'active', 'suspended', 'deleted') 
    DEFAULT 'pending_verification',
  
  -- タイムスタンプ
  verified_at TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,
  
  -- インデックス
  INDEX idx_ulid (ulid),
  INDEX idx_email (email),
  INDEX idx_username (username),
  INDEX idx_status (status),
  INDEX idx_provider (provider, provider_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID（auto increment）
- `ulid`: 外部公開用ID（26文字）
- `email`: メールアドレス（ユニーク）
- `username`: ユーザー名（ユニーク、初期値はメールアドレス）
- `password_hash`: パスワードハッシュ（bcrypt、OAuth専用はNULL）
- `provider`: 認証プロバイダー（local, github）
- `provider_id`: OAuth プロバイダーのユーザーID
- `status`: ユーザーステータス
- `verified_at`: メール認証日時
- `deleted_at`: 論理削除日時

### 2. todos - TODO

```sql
CREATE TABLE todos (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  
  -- 所有者
  user_id BIGINT NOT NULL,
  
  -- TODO情報
  title VARCHAR(255) NOT NULL,
  description TEXT,
  
  -- ステータス
  status ENUM('active', 'completed', 'archived') DEFAULT 'active',
  
  -- 期限
  due_date DATE,
  
  -- 完了情報
  completed_at TIMESTAMP NULL,
  
  -- タイムスタンプ
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,
  
  -- 外部キー
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  
  -- インデックス
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_status (status),
  INDEX idx_due_date (due_date),
  INDEX idx_created_at (created_at),
  INDEX idx_completed_at (completed_at),
  INDEX idx_user_status (user_id, status),
  INDEX idx_user_due_date (user_id, due_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `ulid`: 外部公開用ID
- `user_id`: 所有者のユーザーID
- `title`: TODOのタイトル
- `description`: TODOの詳細（Markdown形式）
- `status`: TODO のステータス（active, completed, archived）
- `due_date`: 期限日
- `completed_at`: 完了日時
- `deleted_at`: 論理削除日時

### 3. todo_comments - TODO コメント

```sql
CREATE TABLE todo_comments (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  
  -- 所属
  todo_id BIGINT NOT NULL,
  user_id BIGINT NOT NULL,
  
  -- コメント内容
  content TEXT NOT NULL,
  
  -- タイムスタンプ
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,
  
  -- 外部キー
  FOREIGN KEY (todo_id) REFERENCES todos(id) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  
  -- インデックス
  INDEX idx_ulid (ulid),
  INDEX idx_todo_id (todo_id),
  INDEX idx_user_id (user_id),
  INDEX idx_created_at (created_at),
  INDEX idx_todo_created (todo_id, created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `ulid`: 外部公開用ID
- `todo_id`: 所属するTODOのID
- `user_id`: コメント作成者のユーザーID
- `content`: コメント内容（Markdown形式）
- `deleted_at`: 論理削除日時

**使用例:**
- TODO の進捗報告
- TODO の状況メモ
- TODO の補足情報

### 4. tags - タグ

```sql
CREATE TABLE tags (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  
  -- 所有者
  user_id BIGINT NOT NULL,
  
  -- タグ情報
  name VARCHAR(100) NOT NULL,
  color VARCHAR(7),  -- HEX color code (#RRGGBB)
  
  -- マージ情報
  merged_to_tag_id BIGINT NULL,
  merged_to_ulid CHAR(26) NULL,
  merged_at TIMESTAMP NULL,
  
  -- タイムスタンプ
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,
  
  -- 外部キー
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (merged_to_tag_id) REFERENCES tags(id) ON DELETE SET NULL,
  
  -- ユニーク制約
  UNIQUE KEY uk_user_name (user_id, name, deleted_at),
  
  -- インデックス
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_name (name),
  INDEX idx_merged_to (merged_to_tag_id),
  INDEX idx_merged_to_ulid (merged_to_ulid),
  INDEX idx_user_name (user_id, name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `ulid`: 外部公開用ID
- `user_id`: 所有者のユーザーID
- `name`: タグ名（大文字、ユーザー内でユニーク）
- `color`: タグの色（HEXカラーコード）
- `merged_to_tag_id`: マージ先タグの内部ID
- `merged_to_ulid`: マージ先タグのULID
- `merged_at`: マージ日時
- `deleted_at`: 論理削除日時

**制約:**
- タグ名はユーザー内でユニーク（削除済みを除く）
- タグ名は大文字で保存

### 5. todo_tags - TODO-タグ ジャンクションテーブル

```sql
CREATE TABLE todo_tags (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  
  -- 関連
  todo_id BIGINT NOT NULL,
  tag_id BIGINT NOT NULL,
  
  -- タイムスタンプ
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  -- 外部キー
  FOREIGN KEY (todo_id) REFERENCES todos(id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE,
  
  -- ユニーク制約
  UNIQUE KEY uk_todo_tag (todo_id, tag_id),
  
  -- インデックス
  INDEX idx_todo_id (todo_id),
  INDEX idx_tag_id (tag_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `todo_id`: TODOの内部ID
- `tag_id`: タグの内部ID
- `created_at`: 紐付け日時

**制約:**
- TODO とタグの組み合わせはユニーク

### 6. labels - ラベル

```sql
CREATE TABLE labels (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  
  -- 所有者
  user_id BIGINT NOT NULL,
  
  -- ラベル情報
  name VARCHAR(100) NOT NULL,
  description TEXT,
  
  -- マージ情報
  merged_to_label_id BIGINT NULL,
  merged_to_ulid CHAR(26) NULL,
  merged_at TIMESTAMP NULL,
  
  -- タイムスタンプ
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,
  
  -- 外部キー
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (merged_to_label_id) REFERENCES labels(id) ON DELETE SET NULL,
  
  -- ユニーク制約
  UNIQUE KEY uk_user_name (user_id, name, deleted_at),
  
  -- インデックス
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_name (name),
  INDEX idx_merged_to (merged_to_label_id),
  INDEX idx_merged_to_ulid (merged_to_ulid),
  INDEX idx_user_name (user_id, name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `ulid`: 外部公開用ID
- `user_id`: 所有者のユーザーID
- `name`: ラベル名（ユーザー内でユニーク）
- `description`: ラベルの説明
- `merged_to_label_id`: マージ先ラベルの内部ID
- `merged_to_ulid`: マージ先ラベルのULID
- `merged_at`: マージ日時
- `deleted_at`: 論理削除日時

**制約:**
- ラベル名はユーザー内でユニーク（削除済みを除く）

### 7. label_tags - ラベル-タグ ジャンクションテーブル

```sql
CREATE TABLE label_tags (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  
  -- 関連
  label_id BIGINT NOT NULL,
  tag_id BIGINT NOT NULL,
  
  -- タイムスタンプ
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  -- 外部キー
  FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE,
  
  -- ユニーク制約
  UNIQUE KEY uk_label_tag (label_id, tag_id),
  
  -- インデックス
  INDEX idx_label_id (label_id),
  INDEX idx_tag_id (tag_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `label_id`: ラベルの内部ID
- `tag_id`: タグの内部ID
- `created_at`: 紐付け日時

**制約:**
- ラベルとタグの組み合わせはユニーク

**注意:**
- ラベルは複数のタグを持つ（AND条件で検索）
- 例: ラベル「朝活」= タグ「MORNING」AND「ACTIVITY」

### 8. email_verification_tokens - メール認証トークン

```sql
CREATE TABLE email_verification_tokens (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  
  -- 所属
  user_id BIGINT NOT NULL,
  
  -- トークン情報
  token_hash VARCHAR(255) UNIQUE NOT NULL,
  expires_at TIMESTAMP NOT NULL,
  used_at TIMESTAMP NULL,
  
  -- タイムスタンプ
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  -- 外部キー
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  
  -- インデックス
  INDEX idx_token_hash (token_hash),
  INDEX idx_user_id (user_id),
  INDEX idx_expires_at (expires_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `user_id`: ユーザーID
- `token_hash`: トークンのハッシュ値（SHA-256）
- `expires_at`: トークンの有効期限（24時間）
- `used_at`: トークン使用日時
- `created_at`: トークン作成日時

**制約:**
- トークンは1回のみ使用可能（`used_at` が NULL のみ有効）

### 9. login_attempts - ログイン試行履歴

```sql
CREATE TABLE login_attempts (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  
  -- 試行情報
  ip_address VARCHAR(45) NOT NULL,  -- IPv6対応
  email VARCHAR(255),
  success BOOLEAN DEFAULT FALSE,
  
  -- タイムスタンプ
  attempted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  -- インデックス
  INDEX idx_ip_time (ip_address, attempted_at),
  INDEX idx_email_time (email, attempted_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `ip_address`: IPアドレス（IPv4/IPv6）
- `email`: ログイン試行時のメールアドレス
- `success`: ログイン成功フラグ
- `attempted_at`: 試行日時

**用途:**
- レート制限（5回/分）
- セキュリティ監視
- 不正アクセス検知

### 10. signup_attempts - サインアップ試行履歴

```sql
CREATE TABLE signup_attempts (
  -- 主キー
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  
  -- 試行情報
  ip_address VARCHAR(45) NOT NULL,
  email VARCHAR(255),
  success BOOLEAN DEFAULT FALSE,
  
  -- タイムスタンプ
  attempted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  
  -- インデックス
  INDEX idx_ip_time (ip_address, attempted_at),
  INDEX idx_email_time (email, attempted_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**フィールド説明:**
- `id`: 内部ID
- `ip_address`: IPアドレス
- `email`: サインアップ試行時のメールアドレス
- `success`: サインアップ成功フラグ
- `attempted_at`: 試行日時

**用途:**
- レート制限（3回/時間）
- スパム対策
- 不正登録検知

## データ型の選択理由

### BIGINT vs INT

- **BIGINT を使用**: 将来的なスケールを考慮
- 最大値: 9,223,372,036,854,775,807
- TODO が数億件になっても対応可能

### CHAR(26) for ULID

- ULID は常に26文字
- CHAR(26) は固定長で効率的
- VARCHAR(26) より高速

### VARCHAR vs TEXT

- **VARCHAR**: 制限がある短い文字列（タイトル、名前など）
- **TEXT**: 長い文字列（説明、コメントなど）

### ENUM vs VARCHAR for status

- **ENUM**: 固定された選択肢
- メリット: メモリ効率、型安全性
- デメリット: スキーマ変更が必要

### TIMESTAMP vs DATETIME

- **TIMESTAMP**: タイムゾーン対応
- 範囲: 1970-2038（2038年問題あり）
- MySQL 5.6.4 以降は DATETIME(6) も推奨

## インデックス設計

### 主キーインデックス

- すべてのテーブルに `id` の PRIMARY KEY
- 自動的にクラスタードインデックス

### ユニークインデックス

- `ulid`: 外部アクセス用
- `email`: ログイン用
- `username`: 表示用
- `uk_user_name`: ユーザー内のユニーク制約

### 検索用インデックス

- `user_id`: 所有者でのフィルタ
- `status`: ステータスでのフィルタ
- `due_date`: 期限日でのソート
- `created_at`: 作成日でのソート

### 複合インデックス

- `(user_id, status)`: ユーザーのTODOを状態で絞り込み
- `(user_id, due_date)`: ユーザーのTODOを期限日でソート
- `(todo_id, created_at)`: TODOのコメントを時系列で取得

### インデックスの選択基準

1. WHERE 句で頻繁に使用されるカラム
2. JOIN の結合キー
3. ORDER BY で使用されるカラム
4. 複合インデックスは左から順に使用される

## 文字コード

- **utf8mb4**: 絵文字対応
- **utf8mb4_unicode_ci**: 大文字小文字を区別しない

## ストレージエンジン

- **InnoDB**: トランザクション対応、外部キー対応

## 論理削除の実装

### 削除の種類

1. **論理削除**: `deleted_at` に日時を設定
2. **物理削除**: レコードを削除（マージのみ）

### 論理削除の利点

- データの復元が可能
- 監査ログとして利用可能
- 参照整合性を維持

### 論理削除の実装

```sql
-- 論理削除
UPDATE todos SET deleted_at = CURRENT_TIMESTAMP WHERE id = ?;

-- 削除されていないレコードのみ取得
SELECT * FROM todos WHERE deleted_at IS NULL;

-- 削除されたレコードを含めて取得
SELECT * FROM todos;
```

### ユニーク制約と論理削除

```sql
-- 削除されていないレコードのみユニーク
UNIQUE KEY uk_user_name (user_id, name, deleted_at)
```

- `deleted_at` が NULL の場合のみユニーク制約が有効
- 削除されたレコードは制約対象外

## マージ機能の実装

### マージの仕組み

1. マージ元のタグ/ラベルを更新
   - `merged_to_tag_id` にマージ先のIDを設定
   - `merged_to_ulid` にマージ先のULIDを設定
   - `merged_at` にマージ日時を設定

2. 関連データの更新
   - タグマージ: `todo_tags` と `label_tags` を更新
   - ラベルマージ: `label_tags` を統合（重複排除）

3. リダイレクト
   - 古いULIDでのアクセスは新しいULIDへリダイレクト
   - 再帰的にマージ先を解決（最大10階層）

### マージの例

```sql
-- タグマージ
UPDATE tags
SET merged_to_tag_id = (SELECT id FROM tags WHERE ulid = 'new_ulid'),
    merged_to_ulid = 'new_ulid',
    merged_at = CURRENT_TIMESTAMP
WHERE ulid = 'old_ulid';

-- TODO の紐付け更新
UPDATE todo_tags
SET tag_id = (SELECT id FROM tags WHERE ulid = 'new_ulid')
WHERE tag_id = (SELECT id FROM tags WHERE ulid = 'old_ulid');
```

## データベースサイズの見積もり

### 1ユーザーあたりのデータ量

| テーブル | レコード数 | サイズ/レコード | 合計サイズ |
|---------|-----------|----------------|-----------|
| todos | 1,000 | 1KB | 1MB |
| todo_comments | 2,000 | 500B | 1MB |
| tags | 50 | 200B | 10KB |
| todo_tags | 3,000 | 50B | 150KB |
| labels | 20 | 300B | 6KB |
| label_tags | 60 | 50B | 3KB |

**合計: 約2.2MB / ユーザー**

### 1万ユーザーでの見積もり

- データサイズ: 22GB
- インデックスサイズ: 約10GB（データの50%）
- **合計: 約32GB**

### 10万ユーザーでの見積もり

- データサイズ: 220GB
- インデックスサイズ: 約110GB
- **合計: 約330GB**

## パフォーマンス最適化

### クエリ最適化

1. **適切なインデックスの使用**
   - EXPLAIN で実行計画を確認
   - カバリングインデックスの活用

2. **N+1 問題の回避**
   - JOIN を使用
   - バッチ取得

3. **ページネーション**
   - LIMIT と OFFSET の使用
   - カーソルベースのページネーション

### データベース設定

1. **InnoDB Buffer Pool Size**
   - 物理メモリの70-80%を割り当て

2. **Connection Pool**
   - 適切なコネクション数の設定

3. **Query Cache**
   - MySQL 8.0 では廃止されたため使用しない

## バックアップ戦略

### 1. 定期バックアップ

- **フルバックアップ**: 毎日1回（深夜）
- **増分バックアップ**: 1時間ごと

### 2. バックアップ方式

- **mysqldump**: 論理バックアップ
- **Percona XtraBackup**: 物理バックアップ（推奨）

### 3. リストア時間目標（RTO）

- 1時間以内

### 4. データ損失許容時間（RPO）

- 1時間以内

## マイグレーション管理

### ツール

- **Flyway**: Java ベース
- **Liquibase**: Java ベース
- **golang-migrate**: Go ベース
- **Alembic**: Python ベース

### マイグレーションファイルの命名規則

```
V{version}__{description}.sql
例: V001__create_users_table.sql
    V002__create_todos_table.sql
    V003__add_merged_to_tags.sql
```

### マイグレーションのベストプラクティス

1. **後方互換性を保つ**
   - カラム削除は段階的に実施
   - まず使用停止、後で削除

2. **ロールバック可能に**
   - ダウンマイグレーションを用意

3. **本番前にテスト**
   - ステージング環境で実施

## セキュリティ考慮事項

### 1. SQL インジェクション対策

- プリペアドステートメントの使用
- ORM の使用

### 2. アクセス制御

- ユーザーごとにデータを分離
- WHERE user_id = ? を必ず含める

### 3. 暗号化

- パスワード: bcrypt（cost factor: 12）
- トークン: SHA-256

### 4. 監査ログ

- 重要な操作を記録
- ログの保持期間: 1年

## 将来の拡張

### 1. TODO 共有機能

```sql
CREATE TABLE todo_shares (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  todo_id BIGINT NOT NULL,
  shared_by_user_id BIGINT NOT NULL,
  shared_to_user_id BIGINT NOT NULL,
  permission ENUM('read', 'write') DEFAULT 'read',
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (todo_id) REFERENCES todos(id) ON DELETE CASCADE,
  FOREIGN KEY (shared_by_user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (shared_to_user_id) REFERENCES users(id) ON DELETE CASCADE
);
```

### 2. 添付ファイル

```sql
CREATE TABLE todo_attachments (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  todo_id BIGINT NOT NULL,
  user_id BIGINT NOT NULL,
  filename VARCHAR(255) NOT NULL,
  file_path VARCHAR(512) NOT NULL,
  file_size BIGINT NOT NULL,
  mime_type VARCHAR(100) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (todo_id) REFERENCES todos(id) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);
```

### 3. 通知機能

```sql
CREATE TABLE notifications (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  user_id BIGINT NOT NULL,
  type VARCHAR(50) NOT NULL,
  title VARCHAR(255) NOT NULL,
  content TEXT,
  read_at TIMESTAMP NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);
```

## 関連仕様

- プロジェクト憲章: `constitution.md`
- タグマージ仕様: `tag-merge-spec.md`
- ラベルマージ仕様: `label-merge-spec.md`
- ホーム画面仕様: `home-spec.md`

---

**Version**: 1.0.0
**Created**: 2026-01-11
**Last Updated**: 2026-01-11
