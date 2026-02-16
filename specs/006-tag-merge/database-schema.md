# Tag Merge - Database Schema Documentation

## Overview

このドキュメントは、タグマージ機能に関連するデータベーススキーマの変更を説明します。

---

## Schema Changes

### 1. tags テーブル

#### 追加カラム

| Column Name | Data Type | Nullable | Default | Description |
|-------------|-----------|----------|---------|-------------|
| merged_to_ulid | VARCHAR(26) | YES | NULL | マージ先タグのULID |
| merged_at | DATETIME | YES | NULL | マージ日時 |

#### インデックス

| Index Name | Columns | Type | Purpose |
|------------|---------|------|---------|
| idx_tags_merged_at | merged_at | BTREE | マージ済みタグのフィルタリング高速化 |

#### 完全なテーブル定義

```sql
CREATE TABLE tags (
  id INT AUTO_INCREMENT PRIMARY KEY,
  ulid VARCHAR(26) NOT NULL UNIQUE,
  owner_id INT NOT NULL,
  name VARCHAR(50) NOT NULL,
  color VARCHAR(7),
  merged_to_ulid VARCHAR(26),
  merged_at DATETIME,
  created_at DATETIME NOT NULL,
  updated_at DATETIME NOT NULL,
  
  FOREIGN KEY (owner_id) REFERENCES users(id) ON DELETE CASCADE,
  
  INDEX idx_tags_owner_id (owner_id),
  INDEX idx_tags_ulid (ulid),
  INDEX idx_tags_merged_at (merged_at),
  
  UNIQUE KEY unique_tag_name_per_user (owner_id, name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

#### カラム詳細

**merged_to_ulid**:
- **目的**: タグがマージされた場合、マージ先のタグULIDを保存
- **NULL値**: タグがマージされていない場合はNULL
- **制約**: 
  - 外部キー制約なし（マージチェーンの循環参照を避けるため）
  - アプリケーションレベルで整合性を保証
- **使用例**:
  ```sql
  -- マージ済みタグを除外
  SELECT * FROM tags WHERE owner_id = 1 AND merged_at IS NULL;
  
  -- マージ先を追跡
  SELECT * FROM tags WHERE ulid = (SELECT merged_to_ulid FROM tags WHERE ulid = '01KHFFS79PW4SBJK3MGAQ8EW9C');
  ```

**merged_at**:
- **目的**: タグがマージされた日時を記録
- **NULL値**: タグがマージされていない場合はNULL
- **データ型**: DATETIME（MySQL）
- **使用例**:
  ```sql
  -- 最近マージされたタグを検索
  SELECT * FROM tags WHERE merged_at > DATE_SUB(NOW(), INTERVAL 7 DAY);
  
  -- マージ済みタグの数をカウント
  SELECT COUNT(*) FROM tags WHERE merged_at IS NOT NULL;
  ```

---

### 2. labels テーブル

#### 追加カラム

| Column Name | Data Type | Nullable | Default | Description |
|-------------|-----------|----------|---------|-------------|
| merged_to_ulid | VARCHAR(26) | YES | NULL | マージ先ラベルのULID |
| merged_at | DATETIME | YES | NULL | マージ日時 |

#### インデックス

| Index Name | Columns | Type | Purpose |
|------------|---------|------|---------|
| idx_labels_merged_at | merged_at | BTREE | マージ済みラベルのフィルタリング高速化 |

#### 完全なテーブル定義

```sql
CREATE TABLE labels (
  id INT AUTO_INCREMENT PRIMARY KEY,
  ulid VARCHAR(26) NOT NULL UNIQUE,
  owner_id INT NOT NULL,
  name VARCHAR(100) NOT NULL,
  description TEXT,
  merged_to_ulid VARCHAR(26),
  merged_at DATETIME,
  created_at DATETIME NOT NULL,
  updated_at DATETIME NOT NULL,
  
  FOREIGN KEY (owner_id) REFERENCES users(id) ON DELETE CASCADE,
  
  INDEX idx_labels_owner_id (owner_id),
  INDEX idx_labels_ulid (ulid),
  INDEX idx_labels_merged_at (merged_at),
  
  UNIQUE KEY unique_label_name_per_user (owner_id, name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
```

**注**: labelsテーブルの`merged_to_ulid`と`merged_at`カラムは、将来的なラベルマージ機能のために追加されましたが、現在のPhase 6では使用されていません。

---

## Migration History

### Migration 1: Add merged_at columns (INTEGER型)

**File**: `db/migrate/20260209114321_add-merged-at-to-tags-and-labels.lisp`

**Date**: 2026-02-09

**Changes**:
- `tags`テーブルに`merged_at`カラム追加（INTEGER型）
- `labels`テーブルに`merged_at`カラム追加（INTEGER型）
- インデックス追加

```lisp
(defmigration "20260209114321_add-merged-at-to-tags-and-labels"
  (:up #'(lambda (connection)
           ;; Add merged-at column to tags table
           (add-column connection
                       :table "tags"
                       :columns '(("merged-at" :type :integer
                                               :not-null nil)))
           
           ;; Add index for merged-at in tags
           (add-index connection
                      :table "tags"
                      :index "idx-tags-merged-at"
                      :columns '("merged-at"))
           
           ;; Add merged-at column to labels table
           (add-column connection
                       :table "labels"
                       :columns '(("merged-at" :type :integer
                                               :not-null nil)))
           
           ;; Add index for merged-at in labels
           (add-index connection
                      :table "labels"
                      :index "idx-labels-merged-at"
                      :columns '("merged-at")))
   :down #'(lambda (connection)
             ;; Drop columns
             (drop-column connection :table "tags" :column "merged-at")
             (drop-column connection :table "labels" :column "merged-at"))))
```

### Migration 2: Change merged_at type to DATETIME

**File**: `db/migrate/20260216105238_change-merged-at-type-to-datetime.lisp`

**Date**: 2026-02-16

**Reason**: 
- INTEGER型（Unix timestamp）では範囲外エラーが発生
- DATETIME型の方がMySQLの標準的な使い方
- フレームワークがDATETIME型を自動的に処理

**Changes**:
1. 既存の`merged_at`カラムとインデックスを削除
2. DATETIME型で`merged_at`カラムを再作成
3. インデックスを再作成

```lisp
(defmigration "20260216105238_change-merged-at-type-to-datetime"
  (:up #'(lambda (connection)
           ;; Drop merged-at column from tags table
           (drop-column connection
                        :table "tags"
                        :column "merged-at")
           
           ;; Add merged-at column with datetime type to tags table
           (add-column connection
                       :table "tags"
                       :columns '(("merged-at" :type :datetime
                                               :not-null nil)))
           
           ;; Add index for merged-at in tags
           (add-index connection
                      :table "tags"
                      :index "idx-tags-merged-at"
                      :columns '("merged-at"))
           
           ;; Drop merged-at column from labels table
           (drop-column connection
                        :table "labels"
                        :column "merged-at")
           
           ;; Add merged-at column with datetime type to labels table
           (add-column connection
                       :table "labels"
                       :columns '(("merged-at" :type :datetime
                                               :not-null nil)))
           
           ;; Add index for merged-at in labels
           (add-index connection
                      :table "labels"
                      :index "idx-labels-merged-at"
                      :columns '("merged-at")))
   :down #'(lambda (connection)
             ;; Rollback logic (revert to INTEGER type)
             ...)))
```

---

## Relationships

### Tag Merge Relationships

```
tags
├─ merged_to_ulid → tags.ulid (self-reference, no FK constraint)
│
├─ owner_id → users.id (FK constraint)
│
└─ Used by:
   ├─ todo_tags.tag_id → tags.id (FK constraint)
   └─ label_tags.tag_id → tags.id (FK constraint)
```

**注意点**:
- `merged_to_ulid`には外部キー制約を設定していません
- これはマージチェーン（A→B→C）の処理を簡単にするため
- アプリケーションレベルで整合性を保証

---

## Queries

### Common Queries

#### 1. アクティブなタグを取得（マージ済みを除外）

```sql
SELECT * FROM tags 
WHERE owner_id = ? 
  AND merged_at IS NULL 
ORDER BY name ASC;
```

**インデックス使用**: `idx_tags_owner_id`, `idx_tags_merged_at`

#### 2. マージ先タグを取得

```sql
SELECT * FROM tags 
WHERE ulid = (
  SELECT merged_to_ulid 
  FROM tags 
  WHERE ulid = ?
);
```

#### 3. マージチェーンを解決

```sql
-- 再帰的CTEを使用（MySQL 8.0+）
WITH RECURSIVE merge_chain AS (
  -- ベースケース
  SELECT ulid, merged_to_ulid, 1 as depth
  FROM tags
  WHERE ulid = ?
  
  UNION ALL
  
  -- 再帰ケース
  SELECT t.ulid, t.merged_to_ulid, mc.depth + 1
  FROM tags t
  INNER JOIN merge_chain mc ON t.ulid = mc.merged_to_ulid
  WHERE mc.depth < 10  -- 最大10階層
)
SELECT ulid, merged_to_ulid FROM merge_chain
ORDER BY depth DESC
LIMIT 1;
```

**注**: アプリケーションコードでは、より効率的な反復的アプローチを使用しています。

#### 4. タグに関連するTODO数を集計

```sql
SELECT t.ulid, t.name, 
       COUNT(tt.todo_id) as todo_count,
       SUM(CASE WHEN todos.status = 'completed' THEN 1 ELSE 0 END) as completed_count,
       SUM(CASE WHEN todos.status = 'active' THEN 1 ELSE 0 END) as active_count
FROM tags t
LEFT JOIN todo_tags tt ON t.id = tt.tag_id
LEFT JOIN todos ON tt.todo_id = todos.id
WHERE t.owner_id = ? AND t.merged_at IS NULL
GROUP BY t.id, t.ulid, t.name;
```

---

## Performance Considerations

### Index Strategy

1. **idx_tags_merged_at**:
   - マージ済みタグのフィルタリングを高速化
   - `WHERE merged_at IS NULL`クエリで使用
   - カーディナリティは低いが、頻繁に使用される

2. **idx_tags_owner_id**:
   - ユーザーごとのタグ取得を高速化
   - 既存のインデックス

3. **複合インデックスの検討**:
   - `(owner_id, merged_at)`の複合インデックスを追加することで、さらなる高速化が可能
   - ただし、現在のクエリパフォーマンスで十分なため、今後の検討事項

### Query Optimization

1. **マージ済みタグの除外**:
   ```sql
   -- 効率的
   WHERE merged_at IS NULL
   
   -- 非効率的（インデックスが使えない）
   WHERE merged_to_ulid IS NULL
   ```

2. **バッチ処理**:
   - 複数のソースタグをマージする際、各タグの処理を個別に実行
   - トランザクションで一括コミット

---

## Data Integrity

### Constraints

1. **NULL constraints**:
   - `merged_to_ulid`: NULL許可（デフォルトはマージされていない）
   - `merged_at`: NULL許可（デフォルトはマージされていない）

2. **Foreign Key constraints**:
   - `merged_to_ulid`には外部キー制約なし
   - アプリケーションレベルで整合性を保証

3. **Unique constraints**:
   - 既存の`(owner_id, name)`の一意性制約を維持

### Data Validation

アプリケーションレベルでの検証:
1. `merged_to_ulid`が存在するタグのULIDであることを確認
2. マージチェーンが循環していないことを確認
3. マージ済みタグ（`merged_at`がNULLでない）を再度マージしないことを確認

---

## Backup and Recovery

### Backup Strategy

マージ操作は不可逆であるため、以下のバックアップ戦略を推奨：

1. **操作前のバックアップ**:
   ```sql
   -- マージ前のタグ状態をバックアップ
   CREATE TABLE tags_backup_20260216 AS SELECT * FROM tags;
   CREATE TABLE todo_tags_backup_20260216 AS SELECT * FROM todo_tags;
   CREATE TABLE label_tags_backup_20260216 AS SELECT * FROM label_tags;
   ```

2. **定期的なバックアップ**:
   - 日次バックアップ
   - トランザクションログのバックアップ

### Recovery Procedure

マージ操作を元に戻す必要がある場合（非推奨）:

1. バックアップから関連レコードを復元
2. マージされたタグの`merged_to_ulid`と`merged_at`をNULLに設定
3. 移行されたtodo_tagsとlabel_tagsを元に戻す

**注意**: この操作は複雑でエラーが発生しやすいため、可能な限り避けるべきです。

---

## Monitoring

### Metrics to Monitor

1. **マージ済みタグの数**:
   ```sql
   SELECT COUNT(*) FROM tags WHERE merged_at IS NOT NULL;
   ```

2. **マージチェーンの深さ**:
   - アプリケーションログで追跡
   - 深さが10に近づいているタグを警告

3. **マージ操作の頻度**:
   - アプリケーションログで追跡
   - 異常な頻度の場合は調査

---

## Migration Checklist

マイグレーション実行時のチェックリスト:

- [ ] 本番データベースのバックアップ取得
- [ ] ステージング環境でマイグレーションテスト
- [ ] マイグレーション実行計画の作成
- [ ] ダウンタイムの見積もり
- [ ] ロールバック手順の準備
- [ ] マイグレーション実行
- [ ] データ整合性チェック
- [ ] アプリケーション動作確認

---

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-02-09 | 1.0.0 | 初版作成（INTEGER型） | AI Assistant |
| 2026-02-16 | 1.1.0 | DATETIME型に変更 | AI Assistant |

---

## References

- [Specification](./specification.md) - タグマージ機能仕様書
- [API Documentation](./api-documentation.md) - API仕様書
- [Tasks](./tasks.md) - 実装タスク
