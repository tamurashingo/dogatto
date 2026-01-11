# ラベルマージ機能仕様

## 概要

ラベルの統合・名称変更をサポートするマージ機能。誤入力の修正やラベル整理時に、古いラベルIDへのアクセスを新しいラベルへ自動リダイレクトすることで、既存のリンクや参照を保持する。

## ラベルとタグの関係

### ラベルの定義

ラベルは**複数のタグの組み合わせ**を表現する検索条件のショートカット：
- ラベル「朝活」= タグ「#MORNING」AND「#ACTIVITY」
- ラベル「筋トレ」= タグ「#WORKOUT」AND「#TUESDAY」

### データフロー

```
TODO ─→ TAG ←─ LABEL
        (所属)    (参照)
```

- TODO は TAG に直接紐付く（todo_tags）
- LABEL は TAG を参照する（label_tags）
- LABEL から TODO への直接の紐付けはない

### 検索の仕組み

```sql
-- ラベル「朝活」でTODO検索
SELECT DISTINCT t.* 
FROM todos t
INNER JOIN todo_tags tt1 ON t.id = tt1.todo_id
INNER JOIN todo_tags tt2 ON t.id = tt2.todo_id
WHERE tt1.tag_id IN (SELECT tag_id FROM label_tags WHERE label_id = :label_id)
  AND tt2.tag_id IN (SELECT tag_id FROM label_tags WHERE label_id = :label_id)
```

## 目的

### ユースケース

1. **誤入力の修正**
   - 例: `朝かつ` → `朝活` への修正
   - ラベルに所属するタグは新しいラベルに引き継がれる

2. **ラベル統合**
   - 例: `ワークアウト` + `筋トレ` → `トレーニング`
   - 両ラベルのタグを統合

3. **ラベル名の変更**
   - 例: `OLD-LABEL` → `NEW-LABEL`
   - 古いラベルIDでのアクセスは新しいラベルへリダイレクト

## なぜマージが必要か

### 問題点

ラベルには ULID が割り振られており、外部からのアクセスは ULID ベース：
- URL: `/labels/{ulid}`
- API: `/api/labels/{ulid}`
- TODO検索: `/api/todos?label_ulid={ulid}`

単純にラベルを削除・再作成すると：
- 古い ULID でのアクセスが 404 エラー
- ブックマークやリンクが無効化
- 共有リンクが切れる

### 解決策

マージ機能により：
- 古い ULID へのアクセスを新しい ULID へ自動リダイレクト
- 既存の参照を保持
- ユーザー体験の向上

## マージの種類

### 1. 既存ラベルへのマージ

**シナリオ:**
- 誤入力を既存の正しいラベルに統合

**例:**
```
マージ前:
- 朝かつ (ulid: 01A...) → [MORNING, ACTIVITY]
- 朝活 (ulid: 01B...) → [MORNING, ACTIVITY, EXERCISE]

マージ後:
- 朝かつ (ulid: 01A...) → merged_to: 01B...
- 朝活 (ulid: 01B...) → [MORNING, ACTIVITY, EXERCISE] ← 有効なラベル
```

**操作:**
1. マージ元ラベルを選択: `朝かつ`
2. マージ先ラベルを選択: `朝活` (既存ラベルから選択)
3. 影響確認:
   - 元のラベルでヒットするTODO数: 5件
   - 新しいラベルでヒットするTODO数: 12件
4. 確認して実行

### 2. 新規ラベルへのマージ

**シナリオ:**
- 複数の古いラベルを新しいラベルに統合

**例:**
```
マージ前:
- ワークアウト (ulid: 01A...) → [WORKOUT, TUESDAY]
- 筋トレ (ulid: 01B...) → [WORKOUT, STRENGTH]

マージ後:
- ワークアウト (ulid: 01A...) → merged_to: 01C...
- 筋トレ (ulid: 01B...) → merged_to: 01C...
- トレーニング (ulid: 01C...) → [WORKOUT, TUESDAY, STRENGTH] ← 新規作成
```

**タグの統合:**
- マージ元ラベルのすべてのタグを収集
- 重複を排除
- 新しいラベルに紐付け

**操作:**
1. マージ元ラベルを選択: `ワークアウト`, `筋トレ`
2. 新しいラベル名を入力: `トレーニング`
3. 統合されるタグを確認: [WORKOUT, TUESDAY, STRENGTH]
4. 影響確認:
   - ワークアウトでヒットするTODO数: 8件
   - 筋トレでヒットするTODO数: 6件
   - トレーニング（統合後）でヒットするTODO数: 10件（重複除く）
5. 確認して実行

## マージの制約

### 1. マージ回数の制限

**ルール:**
- マージされたラベルは再マージ不可
- マージ先ラベルはマージされていなければ再マージ可能

**理由:**
- マージチェーンの複雑化を防ぐ
- リダイレクトループの防止
- パフォーマンスの維持

**例（許可される）:**
```
朝かつ → 朝活  ✓ (初回マージ)
朝活 → DAILY   ✓ (朝活はマージされていない)
```

**例（許可されない）:**
```
朝かつ → 朝活  ✓
朝かつ → DAILY ✗ (朝かつは既にマージ済み)
```

### 2. マージ先の検証

**必須条件:**
- マージ先ラベルはマージされていないこと
- マージ先ラベルは削除されていないこと
- マージ元とマージ先は異なるラベルであること

### 3. タグとの違い

| 項目 | タグマージ | ラベルマージ |
|------|-----------|-------------|
| TODO への影響 | todo_tags 更新 | 影響なし |
| 子要素の扱い | なし | タグの統合 |
| 重複処理 | なし | タグの重複排除 |
| 影響範囲表示 | TODO数、ラベル数 | TODO数（各ラベル、統合後） |

## データモデル

### Labelsテーブル

```sql
CREATE TABLE labels (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  user_id BIGINT NOT NULL,
  name VARCHAR(100) NOT NULL,
  description TEXT,
  merged_to_label_id BIGINT NULL,  -- マージ先のラベルID（内部ID）
  merged_to_ulid CHAR(26) NULL,    -- マージ先のラベルULID（検索用）
  merged_at TIMESTAMP NULL,        -- マージ日時
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,       -- 論理削除（マージ時は使用しない）
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (merged_to_label_id) REFERENCES labels(id) ON DELETE SET NULL,
  UNIQUE KEY uk_user_name (user_id, name),
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_name (name),
  INDEX idx_merged_to (merged_to_label_id),
  INDEX idx_merged_to_ulid (merged_to_ulid)
);
```

### Label_Tagsテーブル（中間テーブル）

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

## マージ処理フロー

### パターン1: 既存ラベルへのマージ

```
入力:
- source_ulid: "01A..." (朝かつ)
- target_ulid: "01B..." (朝活)

処理:
1. バリデーション
   - sourceラベルの存在確認
   - targetラベルの存在確認
   - sourceラベルが未マージであることを確認
   - targetラベルが未マージであることを確認
   - source ≠ target の確認

2. 影響範囲の計算
   - sourceラベルのタグ取得
   - targetラベルのタグ取得
   - sourceラベルでヒットするTODO数算出
   - targetラベルでヒットするTODO数算出

3. トランザクション開始

4. タグの統合
   -- sourceラベルのタグをtargetラベルに追加（重複排除）
   INSERT IGNORE INTO label_tags (label_id, tag_id)
   SELECT 
     (SELECT id FROM labels WHERE ulid = '01B...') as label_id,
     tag_id
   FROM label_tags
   WHERE label_id = (SELECT id FROM labels WHERE ulid = '01A...')

5. ラベルのマージ設定
   UPDATE labels
   SET merged_to_label_id = (SELECT id FROM labels WHERE ulid = '01B...'),
       merged_to_ulid = '01B...',
       merged_at = CURRENT_TIMESTAMP
   WHERE ulid = '01A...'

6. トランザクションコミット

結果:
- 朝かつ → 朝活 へマージ
- 朝活はすべてのタグを持つ
- 朝かつへのアクセスは朝活へリダイレクト
- TODOの紐付けは変更なし（タグとの関係は維持）
```

### パターン2: 新規ラベルへのマージ

```
入力:
- source_ulids: ["01A...", "01B..."] (ワークアウト, 筋トレ)
- new_label_name: "トレーニング"

処理:
1. バリデーション
   - 全sourceラベルの存在確認
   - 全sourceラベルが未マージであることを確認
   - 新しいラベル名の重複確認

2. 影響範囲の計算
   - 各sourceラベルのタグ取得
   - 統合後のタグリスト作成（重複排除）
   - 各sourceラベルでヒットするTODO数算出
   - 統合後のラベルでヒットするTODO数算出

3. トランザクション開始

4. 新しいラベルの作成
   INSERT INTO labels (ulid, user_id, name, description, ...)
   VALUES (generate_ulid(), user_id, 'トレーニング', '', ...)

5. タグの統合
   -- 全sourceラベルのタグを新ラベルに紐付け（重複排除）
   INSERT IGNORE INTO label_tags (label_id, tag_id)
   SELECT 
     (SELECT id FROM labels WHERE ulid = :new_ulid) as label_id,
     DISTINCT tag_id
   FROM label_tags
   WHERE label_id IN (
     SELECT id FROM labels WHERE ulid IN ('01A...', '01B...')
   )

6. 各sourceラベルのマージ設定
   UPDATE labels
   SET merged_to_label_id = (SELECT id FROM labels WHERE ulid = :new_ulid),
       merged_to_ulid = :new_ulid,
       merged_at = CURRENT_TIMESTAMP
   WHERE ulid IN ('01A...', '01B...')

7. トランザクションコミット

結果:
- ワークアウト → トレーニング へマージ
- 筋トレ → トレーニング へマージ
- トレーニングはすべてのタグを持つ（重複排除済み）
- 古いラベルへのアクセスはトレーニングへリダイレクト
```

## 影響範囲の計算

### TODO数の算出

**単一ラベルのTODO数:**
```sql
-- ラベルのすべてのタグを含むTODOを検索
SELECT COUNT(DISTINCT t.id) as todo_count
FROM todos t
WHERE t.id IN (
  -- ラベルに所属する各タグでフィルタ（AND条件）
  SELECT tt.todo_id
  FROM todo_tags tt
  WHERE tt.tag_id IN (
    SELECT tag_id FROM label_tags WHERE label_id = :label_id
  )
  GROUP BY tt.todo_id
  HAVING COUNT(DISTINCT tt.tag_id) = (
    SELECT COUNT(*) FROM label_tags WHERE label_id = :label_id
  )
)
```

**統合後のラベルのTODO数:**
```sql
-- 統合後のタグリストを含むTODOを検索
SELECT COUNT(DISTINCT t.id) as todo_count
FROM todos t
WHERE t.id IN (
  SELECT tt.todo_id
  FROM todo_tags tt
  WHERE tt.tag_id IN (:merged_tag_ids)
  GROUP BY tt.todo_id
  HAVING COUNT(DISTINCT tt.tag_id) = :merged_tag_count
)
```

## リダイレクト処理

### 単一マージのリダイレクト

```
アクセス: GET /api/labels/01A... (朝かつ)

処理:
1. ラベル取得: SELECT * FROM labels WHERE ulid = '01A...'
2. マージチェック: merged_to_ulid IS NOT NULL
3. リダイレクト: merged_to_ulid = '01B...'
4. レスポンス:
   - HTTPステータス: 301 Moved Permanently
   - Location: /api/labels/01B...
   または
   - 直接新しいラベル情報を返す（APIの場合）
```

### 連鎖マージのリダイレクト

**シナリオ:**
```
朝かつ → 朝活 → DAILY
(01A...)  (01B...)  (01C...)
```

**アクセス: GET /api/labels/01A...**

```sql
-- 再帰的にマージ先を取得
WITH RECURSIVE label_chain AS (
  -- 初期ラベル
  SELECT id, ulid, name, merged_to_label_id, merged_to_ulid, 0 as depth
  FROM labels
  WHERE ulid = '01A...'
  
  UNION ALL
  
  -- マージ先を再帰的に取得
  SELECT l.id, l.ulid, l.name, l.merged_to_label_id, l.merged_to_ulid, lc.depth + 1
  FROM labels l
  INNER JOIN label_chain lc ON l.id = lc.merged_to_label_id
  WHERE lc.merged_to_label_id IS NOT NULL
    AND lc.depth < 10  -- 無限ループ防止
)
SELECT * FROM label_chain
ORDER BY depth DESC
LIMIT 1;
```

**結果:**
- 最終的なマージ先: DAILY (01C...)
- リダイレクト: 01A... → 01C...

## API仕様

### 1. ラベルマージ（既存ラベルへ）

**エンドポイント:** `POST /api/labels/merge`

**リクエスト:**
```json
{
  "source_ulids": ["01ARZ3NDEKTSV4RRFFQ69G5FAV"],
  "target_ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV"
}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "merged_labels": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "朝かつ",
        "todo_count": 5,
        "merged_to": {
          "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
          "name": "朝活"
        },
        "merged_at": "2026-01-11T08:00:00Z"
      }
    ],
    "target_label": {
      "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "朝活",
      "description": "朝の活動",
      "tags": [
        {"ulid": "01C...", "name": "MORNING"},
        {"ulid": "01D...", "name": "ACTIVITY"},
        {"ulid": "01E...", "name": "EXERCISE"}
      ],
      "todo_count": 12
    }
  }
}
```

**ステータスコード:**
- 200: マージ成功
- 400: バリデーションエラー
- 401: 認証エラー
- 404: ラベルが見つからない
- 409: マージ制約違反（既にマージ済みなど）
- 500: サーバーエラー

### 2. ラベルマージ（新規ラベルへ）

**エンドポイント:** `POST /api/labels/merge-to-new`

**リクエスト:**
```json
{
  "source_ulids": [
    "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "01BRZ3NDEKTSV4RRFFQ69G5FAV"
  ],
  "new_label": {
    "name": "トレーニング",
    "description": "筋トレとワークアウトの統合"
  }
}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "merged_labels": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "ワークアウト",
        "todo_count": 8,
        "tags": [
          {"ulid": "01C...", "name": "WORKOUT"},
          {"ulid": "01D...", "name": "TUESDAY"}
        ],
        "merged_to": {
          "ulid": "01ERZ3NDEKTSV4RRFFQ69G5FAV",
          "name": "トレーニング"
        },
        "merged_at": "2026-01-11T08:00:00Z"
      },
      {
        "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "筋トレ",
        "todo_count": 6,
        "tags": [
          {"ulid": "01C...", "name": "WORKOUT"},
          {"ulid": "01E...", "name": "STRENGTH"}
        ],
        "merged_to": {
          "ulid": "01ERZ3NDEKTSV4RRFFQ69G5FAV",
          "name": "トレーニング"
        },
        "merged_at": "2026-01-11T08:00:00Z"
      }
    ],
    "new_label": {
      "ulid": "01ERZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "トレーニング",
      "description": "筋トレとワークアウトの統合",
      "tags": [
        {"ulid": "01C...", "name": "WORKOUT"},
        {"ulid": "01D...", "name": "TUESDAY"},
        {"ulid": "01E...", "name": "STRENGTH"}
      ],
      "todo_count": 10
    }
  }
}
```

### 3. マージ影響範囲の事前確認

**エンドポイント:** `POST /api/labels/merge-preview`

**リクエスト:**
```json
{
  "source_ulids": [
    "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "01BRZ3NDEKTSV4RRFFQ69G5FAV"
  ],
  "target_ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV"
}
```

**レスポンス:**
```json
{
  "status": "success",
  "data": {
    "source_labels": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "ワークアウト",
        "todo_count": 8,
        "tags": ["WORKOUT", "TUESDAY"]
      },
      {
        "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "筋トレ",
        "todo_count": 6,
        "tags": ["WORKOUT", "STRENGTH"]
      }
    ],
    "target_label": {
      "ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "トレーニング",
      "current_todo_count": 5,
      "merged_todo_count": 10,
      "current_tags": ["TRAINING"],
      "merged_tags": ["WORKOUT", "TUESDAY", "STRENGTH", "TRAINING"]
    }
  }
}
```

## UI仕様

### ラベルマージ画面

**URL:** `/labels/merge`

**画面構成:**
```
+--------------------------------------------------+
| ラベルのマージ                                    |
+--------------------------------------------------+
| マージ方法を選択:                                 |
| ( ) 既存のラベルにマージ                          |
| (•) 新しいラベルを作成してマージ                  |
+--------------------------------------------------+
| マージ元ラベル: (複数選択可)                      |
| [×] 朝かつ (5件のTODO)                           |
|     タグ: MORNING, ACTIVITY                      |
| [×] ワークアウト (8件のTODO)                     |
|     タグ: WORKOUT, TUESDAY                       |
| [ ] 筋トレ (6件のTODO)                           |
|     タグ: WORKOUT, STRENGTH                      |
+--------------------------------------------------+
| ▼ 既存ラベルにマージの場合                        |
| マージ先ラベル:                                   |
| [朝活 (12件のTODO) ▼] (ドロップダウン)           |
+--------------------------------------------------+
| ▼ 新しいラベルを作成の場合                        |
| 新しいラベル名:                                   |
| [トレーニング__________________]                  |
| 説明:                                            |
| [筋トレとワークアウトの統合_____]                 |
+--------------------------------------------------+
| マージ後の状態:                                   |
| 統合されるタグ: MORNING, ACTIVITY, WORKOUT       |
| 影響を受けるTODO:                                |
| - 朝かつ: 5件                                    |
| - ワークアウト: 8件                               |
| - 統合後: 12件 (重複除く)                        |
+--------------------------------------------------+
|                              [キャンセル] [マージ] |
+--------------------------------------------------+
```

**確認ダイアログ:**
```
+--------------------------------------------------+
| ラベルをマージしますか？                          |
+--------------------------------------------------+
| 以下のラベルをマージします:                       |
| - 朝かつ (5件のTODO) → 朝活                      |
| - ワークアウト (8件のTODO) → トレーニング        |
|                                                  |
| マージ後:                                        |
| - トレーニング: 12件のTODO                       |
| - 統合タグ: MORNING, ACTIVITY, WORKOUT          |
|                                                  |
| この操作は取り消せません。                        |
+--------------------------------------------------+
|                              [キャンセル] [実行]  |
+--------------------------------------------------+
```

## バリデーション

### マージ元ラベル

- 少なくとも1つ選択されていること
- 存在するラベルであること
- ユーザーが所有するラベルであること
- マージされていないラベルであること
- 削除されていないラベルであること

### マージ先ラベル（既存ラベルへマージの場合）

- 選択されていること
- 存在するラベルであること
- ユーザーが所有するラベルであること
- マージされていないラベルであること
- 削除されていないラベルであること
- マージ元ラベルに含まれていないこと

### 新しいラベル（新規ラベルへマージの場合）

- ラベル名が入力されていること
- ラベル名が100文字以内であること
- 同名のラベルが存在しないこと

## セキュリティ要件

### 認証・認可

- ユーザー認証必須
- 自分のラベルのみマージ可能
- 他ユーザーのラベルへのマージ不可

### トランザクション

- すべてのマージ操作はトランザクション内で実行
- 失敗時は全てロールバック
- デッドロック対策

### データ整合性

- 外部キー制約の維持
- マージチェーンの循環参照チェック
- 深さ制限の実施

## パフォーマンス最適化

### インデックス

- `merged_to_label_id` にインデックス
- `merged_to_ulid` にインデックス
- 複合インデックス: `(user_id, merged_to_label_id)`
- label_tags の複合インデックス: `(label_id, tag_id)`

### キャッシュ

- マージ解決結果のキャッシュ（5分間）
- ラベル情報のキャッシュ
- TODO数の計算結果キャッシュ（1分間）
- キャッシュ無効化: マージ実行時

### TODO数の計算

- 非同期で計算（マージ後）
- 初期表示は概算値（キャッシュ）
- 正確な値は背景で計算

## エラーハンドリング

### マージ失敗時

**原因:**
- ラベルが見つからない
- 既にマージされている
- マージ深度の制限超過
- トランザクションエラー

**対応:**
- エラーメッセージ表示
- 詳細なエラー理由
- リトライ可能な場合はリトライボタン

### マージ後のアクセスエラー

**原因:**
- マージ先ラベルが削除された
- データ不整合

**対応:**
- エラーページ表示
- サポートへの連絡案内

## テスト要件

### 単体テスト

- マージロジック
- リダイレクト解決ロジック
- バリデーション
- マージ深度チェック
- TODO数計算ロジック
- タグ重複排除ロジック

### 統合テスト

- マージAPI実行
- タグの統合
- マージ後のラベル取得
- 連鎖マージのリダイレクト
- TODO数の正確性確認

### E2Eテスト

- ラベルマージフロー（既存ラベルへ）
- ラベルマージフロー（新規ラベルへ）
- マージ後のTODO検索
- マージ後のラベルフィルタリング
- 古いラベルURLへのアクセス

## 将来の拡張

- マージの取り消し機能（一定期間内）
- マージ履歴の詳細表示
- バルクマージ機能
- マージ候補の自動提案（類似名ラベル）
- マージ統計情報
- マージログのエクスポート
- タグの選択的マージ（一部のタグのみ引き継ぐ）

## 関連仕様

- プロジェクト憲章: `constitution.md`
- タグマージ仕様: `tag-merge-spec.md`
- タグ管理画面仕様: `tags-spec.md` (今後作成)
- ラベル管理画面仕様: `labels-spec.md` (今後作成)

---

**Version**: 1.0.0
**Created**: 2026-01-11
**Last Updated**: 2026-01-11
