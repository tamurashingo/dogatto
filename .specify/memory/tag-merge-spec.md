# タグマージ機能仕様

## 概要

タグの統合・名称変更をサポートするマージ機能。誤入力の修正やプロジェクト統合時に、古いタグIDへのアクセスを新しいタグへ自動リダイレクトすることで、既存のリンクや参照を保持する。

## 目的

### ユースケース

1. **誤入力の修正**
   - 例: `#MORNIG` → `#MORNING` への修正
   - 既存TODOは新しいタグに自動的に紐付け直される

2. **プロジェクト統合**
   - 例: `#PROJECT-A` + `#PROJECT-B` → `#PROJECT-C`
   - 両プロジェクトのTODOは統合後のタグに紐付く

3. **タグ名の変更**
   - 例: `#OLD-NAME` → `#NEW-NAME`
   - 古いタグIDでのアクセスは新しいタグへリダイレクト

## なぜマージが必要か

### 問題点

タグには ULID が割り振られており、外部からのアクセスは ULID ベース:
- URL: `/tags/{ulid}`
- API: `/api/tags/{ulid}`
- TODO検索: `/api/todos?tag_ulids={ulid}`

単純にタグを削除・再作成すると:
- 古い ULID でのアクセスが 404 エラー
- ブックマークやリンクが無効化
- 他システムとの連携が切れる

### 解決策

マージ機能により:
- 古い ULID へのアクセスを新しい ULID へ自動リダイレクト
- 既存の参照を保持
- ユーザー体験の向上

## マージの種類

### 1. 既存タグへのマージ

**シナリオ:**
- 誤入力を既存の正しいタグに統合

**例:**
```
マージ前:
- MORNIG (ulid: 01A...)
- MORNING (ulid: 01B...)

マージ後:
- MORNIG (ulid: 01A...) → merged_to: 01B...
- MORNING (ulid: 01B...) ← 有効なタグ
```

**操作:**
1. マージ元タグを選択: `MORNIG`
2. マージ先タグを選択: `MORNING` (既存タグから選択)
3. 確認して実行

### 2. 新規タグへのマージ

**シナリオ:**
- 複数の古いタグを新しいタグに統合

**例:**
```
マージ前:
- PROJECT-A (ulid: 01A...)
- PROJECT-B (ulid: 01B...)

マージ後:
- PROJECT-A (ulid: 01A...) → merged_to: 01C...
- PROJECT-B (ulid: 01B...) → merged_to: 01C...
- PROJECT-C (ulid: 01C...) ← 新規作成
```

**操作:**
1. マージ元タグを選択: `PROJECT-A`, `PROJECT-B`
2. 新しいタグ名を入力: `PROJECT-C`
3. 確認して実行

## マージの制約

### 1. マージ回数の制限

**ルール:**
- マージされたタグは再マージ不可
- マージ先タグはマージされていなければ再マージ可能

**理由:**
- マージチェーンの複雑化を防ぐ
- リダイレクトループの防止
- パフォーマンスの維持

**例（許可される）:**
```
MORNIG → MORNING  ✓ (初回マージ)
MORNING → DAILY   ✓ (MORNINGはマージされていない)
```

**例（許可されない）:**
```
MORNIG → MORNING  ✓
MORNIG → DAILY    ✗ (MORNIGは既にマージ済み)
```

### 2. マージ先の検証

**必須条件:**
- マージ先タグはマージされていないこと
- マージ先タグは削除されていないこと
- マージ元とマージ先は異なるタグであること

### 3. 削除との違い

| 操作 | 古いULIDへのアクセス | TODOの紐付き | 復元可能性 |
|------|---------------------|-------------|-----------|
| マージ | 新しいタグへリダイレクト | 新しいタグに移行 | 不可 |
| 削除 | 404エラー | 削除される | 論理削除なら可 |

## データモデル

### Tagsテーブル

```sql
CREATE TABLE tags (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid CHAR(26) UNIQUE NOT NULL,
  user_id BIGINT NOT NULL,
  name VARCHAR(100) NOT NULL,
  color VARCHAR(7),  -- HEX color code
  merged_to_tag_id BIGINT NULL,  -- マージ先のタグID（内部ID）
  merged_to_ulid CHAR(26) NULL,  -- マージ先のタグULID（検索用）
  merged_at TIMESTAMP NULL,      -- マージ日時
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  deleted_at TIMESTAMP NULL,     -- 論理削除（マージ時は使用しない）
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  FOREIGN KEY (merged_to_tag_id) REFERENCES tags(id) ON DELETE SET NULL,
  UNIQUE KEY uk_user_name (user_id, name),
  INDEX idx_ulid (ulid),
  INDEX idx_user_id (user_id),
  INDEX idx_name (name),
  INDEX idx_merged_to (merged_to_tag_id),
  INDEX idx_merged_to_ulid (merged_to_ulid)
);
```

**フィールド説明:**
- `merged_to_tag_id`: マージ先タグの内部ID（JOIN用）
- `merged_to_ulid`: マージ先タグのULID（検索・リダイレクト用）
- `merged_at`: マージ実行日時
- マージされたタグは `name` は保持（履歴として）

## マージ処理フロー

### パターン1: 既存タグへのマージ

```
入力:
- source_ulid: "01A..." (MORNIG)
- target_ulid: "01B..." (MORNING)

処理:
1. バリデーション
   - sourceタグの存在確認
   - targetタグの存在確認
   - sourceタグが未マージであることを確認
   - targetタグが未マージであることを確認
   - source ≠ target の確認

2. トランザクション開始

3. TODOの紐付け更新
   UPDATE todo_tags 
   SET tag_id = (SELECT id FROM tags WHERE ulid = '01B...')
   WHERE tag_id = (SELECT id FROM tags WHERE ulid = '01A...')

4. ラベルの紐付け更新
   UPDATE label_tags
   SET tag_id = (SELECT id FROM tags WHERE ulid = '01B...')
   WHERE tag_id = (SELECT id FROM tags WHERE ulid = '01A...')

5. タグのマージ設定
   UPDATE tags
   SET merged_to_tag_id = (SELECT id FROM tags WHERE ulid = '01B...'),
       merged_to_ulid = '01B...',
       merged_at = CURRENT_TIMESTAMP
   WHERE ulid = '01A...'

6. トランザクションコミット

結果:
- MORNIG → MORNING へマージ
- すべてのTODO/ラベルはMORNINGに紐付く
- MORNIGへのアクセスはMORNINGへリダイレクト
```

### パターン2: 新規タグへのマージ

```
入力:
- source_ulids: ["01A...", "01B..."] (PROJECT-A, PROJECT-B)
- new_tag_name: "PROJECT-C"

処理:
1. バリデーション
   - 全sourceタグの存在確認
   - 全sourceタグが未マージであることを確認
   - 新しいタグ名の重複確認

2. トランザクション開始

3. 新しいタグの作成
   INSERT INTO tags (ulid, user_id, name, color, ...)
   VALUES (generate_ulid(), user_id, 'PROJECT-C', default_color, ...)

4. 各sourceタグに対して:
   a. TODOの紐付け更新
   b. ラベルの紐付け更新
   c. タグのマージ設定

5. トランザクションコミット

結果:
- PROJECT-A → PROJECT-C へマージ
- PROJECT-B → PROJECT-C へマージ
- すべてのTODO/ラベルはPROJECT-Cに紐付く
- PROJECT-A/BへのアクセスはPROJECT-Cへリダイレクト
```

## リダイレクト処理

### 単一マージのリダイレクト

```
アクセス: GET /api/tags/01A... (MORNIG)

処理:
1. タグ取得: SELECT * FROM tags WHERE ulid = '01A...'
2. マージチェック: merged_to_ulid IS NOT NULL
3. リダイレクト: merged_to_ulid = '01B...'
4. レスポンス:
   - HTTPステータス: 301 Moved Permanently
   - Location: /api/tags/01B...
   または
   - 直接新しいタグ情報を返す（APIの場合）
```

### 連鎖マージのリダイレクト

**シナリオ:**
```
MORNIG → MORNING → DAILY
(01A...)   (01B...)   (01C...)
```

**アクセス: GET /api/tags/01A...**

```sql
-- 再帰的にマージ先を取得
WITH RECURSIVE tag_chain AS (
  -- 初期タグ
  SELECT id, ulid, name, merged_to_tag_id, merged_to_ulid, 0 as depth
  FROM tags
  WHERE ulid = '01A...'
  
  UNION ALL
  
  -- マージ先を再帰的に取得
  SELECT t.id, t.ulid, t.name, t.merged_to_tag_id, t.merged_to_ulid, tc.depth + 1
  FROM tags t
  INNER JOIN tag_chain tc ON t.id = tc.merged_to_tag_id
  WHERE tc.merged_to_tag_id IS NOT NULL
    AND tc.depth < 10  -- 無限ループ防止
)
SELECT * FROM tag_chain
ORDER BY depth DESC
LIMIT 1;
```

**結果:**
- 最終的なマージ先: DAILY (01C...)
- リダイレクト: 01A... → 01C...

### リダイレクトの深さ制限

**制限:**
- 最大10階層まで
- 10階層を超える場合はエラー

**理由:**
- 無限ループ防止
- パフォーマンス維持
- データ整合性の問題検出

## API仕様

### 6.1 タグマージ（既存タグへ）

**エンドポイント:** `POST /api/tags/merge`

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
    "merged_tags": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "MORNIG",
        "merged_to": {
          "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
          "name": "MORNING"
        },
        "merged_at": "2026-01-11T08:00:00Z"
      }
    ],
    "target_tag": {
      "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "MORNING",
      "color": "#3B82F6",
      "todo_count": 15
    }
  }
}
```

**レスポンス (失敗):**
```json
{
  "status": "error",
  "error": {
    "code": "MERGE_FAILED",
    "message": "マージに失敗しました",
    "details": {
      "source_ulids": ["マージ元タグは既にマージされています"]
    }
  }
}
```

**ステータスコード:**
- 200: マージ成功
- 400: バリデーションエラー
- 401: 認証エラー
- 404: タグが見つからない
- 409: マージ制約違反（既にマージ済みなど）
- 500: サーバーエラー

### 6.2 タグマージ（新規タグへ）

**エンドポイント:** `POST /api/tags/merge-to-new`

**リクエスト:**
```json
{
  "source_ulids": [
    "01ARZ3NDEKTSV4RRFFQ69G5FAV",
    "01BRZ3NDEKTSV4RRFFQ69G5FAV"
  ],
  "new_tag": {
    "name": "PROJECT-C",
    "color": "#10B981"
  }
}
```

**レスポンス (成功):**
```json
{
  "status": "success",
  "data": {
    "merged_tags": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "PROJECT-A",
        "merged_to": {
          "ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV",
          "name": "PROJECT-C"
        },
        "merged_at": "2026-01-11T08:00:00Z"
      },
      {
        "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "PROJECT-B",
        "merged_to": {
          "ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV",
          "name": "PROJECT-C"
        },
        "merged_at": "2026-01-11T08:00:00Z"
      }
    ],
    "new_tag": {
      "ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "PROJECT-C",
      "color": "#10B981",
      "todo_count": 25
    }
  }
}
```

### 6.3 タグ取得（マージ解決付き）

**エンドポイント:** `GET /api/tags/{ulid}`

**パスパラメータ:**
- `ulid`: タグのULID

**クエリパラメータ:**
- `resolve_merge`: マージを解決するか（デフォルト: true）

**レスポンス (マージされていないタグ):**
```json
{
  "status": "success",
  "data": {
    "tag": {
      "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "MORNING",
      "color": "#3B82F6",
      "todo_count": 15,
      "is_merged": false,
      "created_at": "2026-01-01T00:00:00Z"
    }
  }
}
```

**レスポンス (マージされたタグ、resolve_merge=true):**
```json
{
  "status": "success",
  "data": {
    "tag": {
      "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "MORNING",
      "color": "#3B82F6",
      "todo_count": 15,
      "is_merged": false,
      "created_at": "2026-01-01T00:00:00Z"
    },
    "merged_from": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "MORNIG",
      "merged_at": "2026-01-11T08:00:00Z"
    }
  }
}
```

**レスポンス (マージされたタグ、resolve_merge=false):**
```json
{
  "status": "success",
  "data": {
    "tag": {
      "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "MORNIG",
      "color": "#3B82F6",
      "is_merged": true,
      "merged_to": {
        "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "MORNING"
      },
      "merged_at": "2026-01-11T08:00:00Z",
      "created_at": "2026-01-01T00:00:00Z"
    }
  }
}
```

### 6.4 マージ履歴取得

**エンドポイント:** `GET /api/tags/{ulid}/merge-history`

**レスポンス:**
```json
{
  "status": "success",
  "data": {
    "current_tag": {
      "ulid": "01CRZ3NDEKTSV4RRFFQ69G5FAV",
      "name": "DAILY"
    },
    "merged_from": [
      {
        "ulid": "01ARZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "MORNIG",
        "merged_at": "2026-01-10T08:00:00Z"
      },
      {
        "ulid": "01BRZ3NDEKTSV4RRFFQ69G5FAV",
        "name": "MORNING",
        "merged_at": "2026-01-11T08:00:00Z"
      }
    ]
  }
}
```

## UI仕様

### タグマージ画面

**URL:** `/tags/merge`

**画面構成:**
```
+--------------------------------------------------+
| タグのマージ                                      |
+--------------------------------------------------+
| マージ方法を選択:                                 |
| ( ) 既存のタグにマージ                            |
| (•) 新しいタグを作成してマージ                    |
+--------------------------------------------------+
| マージ元タグ: (複数選択可)                        |
| [×] MORNIG                                       |
| [×] PROJECT-A                                    |
| [ ] MORNING                                      |
| [ ] PROJECT-B                                    |
+--------------------------------------------------+
| ▼ 既存タグにマージの場合                          |
| マージ先タグ:                                     |
| [MORNING ▼] (ドロップダウン)                     |
+--------------------------------------------------+
| ▼ 新しいタグを作成の場合                          |
| 新しいタグ名:                                     |
| [PROJECT-C__________________]                    |
| タグの色:                                         |
| [#10B981] (カラーピッカー)                       |
+--------------------------------------------------+
| 影響を受けるTODO: 25件                           |
| 影響を受けるラベル: 3件                           |
+--------------------------------------------------+
|                              [キャンセル] [マージ] |
+--------------------------------------------------+
```

**確認ダイアログ:**
```
+--------------------------------------------------+
| タグをマージしますか？                            |
+--------------------------------------------------+
| 以下のタグをマージします:                         |
| - MORNIG → MORNING                               |
| - PROJECT-A → PROJECT-C                          |
|                                                  |
| この操作は取り消せません。                        |
| 影響を受けるTODO: 25件                           |
| 影響を受けるラベル: 3件                           |
+--------------------------------------------------+
|                              [キャンセル] [実行]  |
+--------------------------------------------------+
```

## バリデーション

### マージ元タグ

- 少なくとも1つ選択されていること
- 存在するタグであること
- ユーザーが所有するタグであること
- マージされていないタグであること
- 削除されていないタグであること

### マージ先タグ（既存タグへマージの場合）

- 選択されていること
- 存在するタグであること
- ユーザーが所有するタグであること
- マージされていないタグであること
- 削除されていないタグであること
- マージ元タグに含まれていないこと

### 新しいタグ（新規タグへマージの場合）

- タグ名が入力されていること
- タグ名が100文字以内であること
- 大文字に変換後、同名のタグが存在しないこと

## セキュリティ要件

### 認証・認可

- ユーザー認証必須
- 自分のタグのみマージ可能
- 他ユーザーのタグへのマージ不可

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

- `merged_to_tag_id` にインデックス
- `merged_to_ulid` にインデックス
- 複合インデックス: `(user_id, merged_to_tag_id)`

### キャッシュ

- マージ解決結果のキャッシュ（5分間）
- タグ情報のキャッシュ
- キャッシュ無効化: マージ実行時

### バッチ処理

- 大量TODOの紐付け更新は非同期処理
- 進捗通知機能

## エラーハンドリング

### マージ失敗時

**原因:**
- タグが見つからない
- 既にマージされている
- マージ深度の制限超過
- トランザクションエラー

**対応:**
- エラーメッセージ表示
- 詳細なエラー理由
- リトライ可能な場合はリトライボタン

### マージ後のアクセスエラー

**原因:**
- マージ先タグが削除された
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

### 統合テスト

- マージAPI実行
- TODO紐付け更新
- ラベル紐付け更新
- マージ後のタグ取得
- 連鎖マージのリダイレクト

### E2Eテスト

- タグマージフロー（既存タグへ）
- タグマージフロー（新規タグへ）
- マージ後のTODO表示
- マージ後のタグフィルタリング
- 古いタグURLへのアクセス

## 将来の拡張

- マージの取り消し機能（一定期間内）
- マージ履歴の詳細表示
- バルクマージ機能
- マージ候補の自動提案（類似名タグ）
- マージ統計情報
- マージログのエクスポート

## 関連仕様

- プロジェクト憲章: `constitution.md`
- タグ管理画面仕様: `tags-spec.md` (今後作成)
- ラベル管理画面仕様: `labels-spec.md` (今後作成)
- ラベルマージ仕様: `label-merge-spec.md` (同様の仕様)

---

**Version**: 1.0.0
**Created**: 2026-01-11
**Last Updated**: 2026-01-11
