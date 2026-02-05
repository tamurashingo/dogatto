# 005-labels-management 仕様書

## 概要

dogatto（タグベースTODO管理アプリケーション）のラベル管理機能を実装します。
ラベルは複数のタグの組み合わせを表現し、TODO検索の効率化を図ります。

## 目標

ユーザーが以下の操作を実行できるようにする：
- 複数タグの組み合わせをラベルとして保存
- ラベルを使用したTODO検索
- ラベルの管理（作成、編集、削除）
- ラベルによるTODO整理の効率化

## 前提条件

- Phase 1（認証システム）完了
- Phase 2（TODO基本機能）完了
- Phase 3（タグ管理機能）完了
- ユーザーは認証済みであること
- タグが作成されていること

## スコープ

### Phase 5に含まれる機能

✅ **ラベルCRUD操作**：
- ラベルの作成
- ラベルの読み取り（一覧・詳細）
- ラベルの更新（名前、説明、タグ）
- ラベルの削除

✅ **ラベル属性**：
- 名前（必須、ユーザーごとに一意）
- 説明（任意）
- 関連タグ（最低1つ必須）
- TODO数（統計情報）
- 作成日時・更新日時

✅ **ラベルとタグの関連付け**：
- ラベルへの複数タグの割り当て
- タグの追加・削除
- タグの組み合わせによるTODO検索（AND条件）

✅ **ラベルによる検索**：
- ラベル名検索
- タグ名でのラベル検索
- TODO数の推定

✅ **ラベル管理UI**：
- ラベル一覧ページ
- ラベル作成モーダル
- ラベル編集モーダル
- タグドロップダウン
- 検索機能

✅ **ラベル統計**：
- ラベルごとのTODO数（AND条件）
- 使用中/未使用ラベルの分類

### Phase 5に含まれない機能

❌ **高度な機能**（将来のフェーズ）：
- ラベルマージ機能（Phase 6で実装予定）
- ラベルのグループ化
- OR条件のサポート
- ラベルのお気に入り
- ラベルのエクスポート/インポート
- ラベルの履歴表示

## データモデル

### Labelsテーブル

```sql
CREATE TABLE labels (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  ulid VARCHAR(26) NOT NULL UNIQUE,
  owner_id BIGINT NOT NULL,
  name VARCHAR(100) NOT NULL,
  description TEXT,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL,
  FOREIGN KEY (owner_id) REFERENCES users(id) ON DELETE CASCADE,
  UNIQUE KEY unique_user_label (owner_id, name)
);
```

**カラム説明**：
- `id`: 内部ID（自動採番）
- `ulid`: 外部公開用の一意識別子
- `owner_id`: ラベルの所有者（ユーザーID）
- `name`: ラベル名（100文字以内、ユーザーごとに一意）
- `description`: ラベルの説明（任意、最大1000文字）
- `created_at`: 作成日時（Universal Time）
- `updated_at`: 更新日時（Universal Time）

### Label_Tags 中間テーブル

```sql
CREATE TABLE label_tags (
  id BIGINT AUTO_INCREMENT PRIMARY KEY,
  label_id BIGINT NOT NULL,
  tag_id BIGINT NOT NULL,
  created_at BIGINT NOT NULL,
  FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE CASCADE,
  FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE,
  UNIQUE KEY unique_label_tag (label_id, tag_id)
);
```

**カラム説明**：
- `id`: 内部ID（自動採番）
- `label_id`: ラベルID
- `tag_id`: タグID
- `created_at`: 関連付け日時（Universal Time）

**制約**：
- ラベル削除時に関連するlabel_tagsも削除（CASCADE）
- タグ削除時に関連するlabel_tagsも削除（CASCADE）
- 同じラベル-タグの組み合わせは1つのみ（UNIQUE制約）

## APIエンドポイント

### ラベル管理

#### ラベル一覧取得
```
GET /api/v1/labels

Query Parameters:
- page: ページ番号（デフォルト: 1）
- per_page: 1ページあたりの件数（デフォルト: 20、最大: 100）
- sort: ソート項目（name, tag_count, todo_count, updated_at）
- order: ソート順（asc, desc）
- filter: フィルタ（all, used, unused）
- search_mode: 検索モード（label_name, tag_name）
- q: 検索キーワード

Response: 200 OK
{
  "status": "success",
  "data": {
    "labels": [
      {
        "id": 1,
        "ulid": "01HQWE...",
        "name": "朝活",
        "description": "朝の活動タスク",
        "tags": [
          {
            "ulid": "01HQTAG...",
            "name": "MORNING",
            "color": "#F97316"
          },
          {
            "ulid": "01HQTAG2...",
            "name": "ACTIVITY",
            "color": "#3B82F6"
          }
        ],
        "tagCount": 2,
        "todoCount": 5,
        "createdAt": 1234567890,
        "updatedAt": 1234567890
      }
    ],
    "pagination": {
      "currentPage": 1,
      "perPage": 20,
      "totalPages": 3,
      "totalCount": 55
    },
    "stats": {
      "totalLabels": 55,
      "usedLabels": 45,
      "unusedLabels": 10
    }
  }
}
```

#### ラベル作成
```
POST /api/v1/labels

Body: {
  "name": "朝活",
  "description": "朝の活動タスク",
  "tagUlids": [
    "01HQTAG...",
    "01HQTAG2..."
  ]
}

Response: 201 Created
{
  "status": "success",
  "data": {
    "label": { ... }
  }
}
```

**エラーレスポンス**：
- 400: バリデーションエラー（名前が空、タグ未選択など）
- 409: 重複エラー（同名のラベルが既に存在）

#### ラベル詳細取得
```
GET /api/v1/labels/:ulid

Response: 200 OK
{
  "status": "success",
  "data": {
    "label": { ... },
    "todos": [ ... ]  // このラベルでヒットするTODO一覧（AND条件）
  }
}
```

**エラーレスポンス**：
- 404: ラベルが存在しない
- 403: 他ユーザーのラベルへのアクセス

#### ラベル更新
```
PUT /api/v1/labels/:ulid

Body: {
  "name": "朝活動",
  "description": "朝の活動タスク（更新）",
  "tagUlids": [
    "01HQTAG...",
    "01HQTAG2...",
    "01HQTAG3..."
  ]
}

Response: 200 OK
{
  "status": "success",
  "data": {
    "label": { ... }
  }
}
```

**エラーレスポンス**：
- 400: バリデーションエラー
- 404: ラベルが存在しない
- 409: 名前の重複

#### ラベル削除
```
DELETE /api/v1/labels/:ulid

Response: 204 No Content
```

**動作**：
- ラベルを削除すると、関連するlabel_tagsも自動削除される（CASCADE）
- TODOは削除されない
- タグも削除されない

**エラーレスポンス**：
- 404: ラベルが存在しない
- 403: 他ユーザーのラベルの削除試行

### TODO数推定

#### TODO数の推定
```
GET /api/v1/labels/estimate-todo-count

Query Parameters:
- tag_ulids: タグのULIDをカンマ区切り

Response: 200 OK
{
  "status": "success",
  "data": {
    "todoCount": 5,
    "tags": [
      {
        "ulid": "01HQTAG...",
        "name": "MORNING"
      },
      {
        "ulid": "01HQTAG2...",
        "name": "ACTIVITY"
      }
    ]
  }
}
```

**動作**：
- 指定されたタグの組み合わせでヒットするTODO数を計算（AND条件）
- リアルタイム計算に使用

### ラベル使用状況

#### 使用状況取得
```
GET /api/v1/labels/:ulid/usage

Response: 200 OK
{
  "status": "success",
  "data": {
    "label": {
      "ulid": "01HQWE...",
      "name": "朝活",
      "tags": [ ... ]
    },
    "usage": {
      "todoCount": 5,
      "todos": [
        {
          "ulid": "01HQTODO...",
          "title": "体重測定",
          "dueDate": 1234567890
        }
      ]
    }
  }
}
```

### TODOフィルタリング

#### ラベルでTODOを絞り込み
```
GET /api/v1/todos?label=:ulid

Response: 200 OK
{
  "status": "success",
  "data": {
    "todos": [ ... ]  // ラベルの全タグを持つTODO（AND条件）
  }
}
```

**動作**：
- ラベルに関連付けられた**全てのタグを持つ**TODOを返す（AND条件）
- タグフィルタとの併用も可能

## UI/UX要件

### ラベル一覧ページ (`/labels`)

**コンポーネント**：
- ページヘッダーと「ラベルを作成」ボタン
- 検索エリア
  - 検索モード切り替え（ラベル名/タグ名）
  - 検索入力欄
- 統計情報表示（全ラベル数、使用中、未使用）
- ラベルテーブル/カードのグリッド表示：
  - ラベル名
  - タグバッジ（最大5つ、それ以上は+N）
  - TODO数
  - 操作ボタン（編集・削除）
- ページネーション
- ラベルが存在しない場合の空状態

**アクション**：
- 「ラベルを作成」クリック → ラベル作成モーダルを開く
- ラベル行クリック → ラベル詳細ページへ遷移（将来実装）
- 編集ボタンクリック → ラベル編集モーダルを開く
- 削除ボタンクリック → 確認ダイアログ表示

### ラベル作成モーダル

**フォームフィールド**：
- 名前（テキスト入力、必須、最大100文字）
- 説明（テキストエリア、任意、最大1000文字）
- タグ選択（ドロップダウン、最低1つ必須）

**タグドロップダウン**：
- 既存タグから選択
- タグ名で検索可能
- タグの色とTODO数を表示
- 既に選択済みのタグは無効化
- キーボードナビゲーション対応

**プレビュー**：
- 推定TODO数の表示
- リアルタイム計算（Debounce: 500ms）

**バリデーション**：
- 名前が空の場合エラー表示
- 名前が100文字を超える場合エラー表示
- 重複する名前の場合エラー表示
- タグ未選択の場合エラー表示

**アクション**：
- 送信 → ラベル作成、モーダルを閉じる
- キャンセル → 保存せずにモーダルを閉じる

### ラベル編集モーダル

**フォームフィールド**：
- 名前（既存値を表示）
- 説明（既存値を表示）
- タグ選択（既存タグを表示）

**確認ダイアログ（タグ変更時）**：
- 変更前後のタグ表示
- 変更前後のTODO数表示
- 警告メッセージ

## バリデーションルール

### ラベル名
- 必須
- 1〜100文字
- ユーザーごとに一意
- 前後の空白を削除
- 大文字小文字を区別しない一意性

### 説明
- 任意
- 最大1000文字

### タグ
- 最低1つ必須
- ユーザーは自分のタグのみを選択可能

## セキュリティ要件

### 認可
- ユーザーは自分のラベルのみをCRUD可能
- ユーザーは自分のタグのみをラベルに割り当て可能
- 他ユーザーのラベルへのアクセス試行は403 Forbiddenを返す

### 入力検証
- ラベル名のサニタイズ（XSS防止）
- 説明のサニタイズ
- タグULIDの検証

## エラーハンドリング

### 一般的なエラー

**ラベルが見つからない** (404)：
```json
{
  "status": "error",
  "message": "Label not found"
}
```

**ラベル名の重複** (409)：
```json
{
  "status": "error",
  "message": "Label with this name already exists"
}
```

**バリデーションエラー** (400)：
```json
{
  "status": "error",
  "message": "Label name is required"
}
```

**認可エラー** (403)：
```json
{
  "status": "error",
  "message": "Access denied"
}
```

## パフォーマンス考慮事項

### データベース最適化
- labelsテーブルの`owner_id`にインデックス
- label_tagsテーブルの`(label_id, tag_id)`に複合インデックス
- `name`にインデックス（高速なラベル検索）

### クエリ最適化
- JOINを使用して単一クエリでタグとTODO数を取得
- ラベル一覧のタグをバッチ読み込み

### フロントエンド最適化
- 検索のデバウンス（300ms）
- TODO数推定のデバウンス（500ms）
- 検索結果のキャッシュ

## 成功基準

### 機能面
- ✅ ユーザーはラベルを作成、編集、削除できる
- ✅ ユーザーはラベルに複数タグを割り当てられる
- ✅ ユーザーはラベルでTODOを検索できる（AND条件）
- ✅ ラベル統計が正確である
- ✅ すべての認可チェックが機能する

### パフォーマンス
- ✅ ラベル一覧が500ms以内に読み込まれる
- ✅ ラベルによるTODO検索が1秒以内に完了
- ✅ TODO数推定がリアルタイムで表示される

### UX
- ✅ ラベル作成が直感的である
- ✅ タグ選択が使いやすい
- ✅ 検索が高速で正確である
- ✅ モバイル体験がスムーズである

## タイムライン見積もり

- **バックエンド開発**: 2〜3日
  - データベースマイグレーション: 0.5日
  - ラベルモデル & CRUD: 1日
  - Label-Tag関連付け: 0.5日
  - TODO数推定: 0.5日
  - テスト: 0.5日

- **フロントエンド開発**: 3〜4日
  - ラベルAPIクライアント: 0.5日
  - ラベル一覧ページ: 1日
  - ラベル作成/編集モーダル: 1日
  - タグドロップダウン: 0.5日
  - 検索機能: 0.5日
  - 統合: 0.5日

- **テストとバグ修正**: 1〜2日

**合計**: 6〜9日

## 依存関係

- Phase 3（タグ管理機能）が完了していること
- タグが作成されていること

## 次のステップ

Phase 5完了後の候補：
1. Phase 6: ラベルマージ機能
2. Phase 7: TODO検索機能強化
3. Phase 8: TODOコメント機能

---

**Version**: 1.0.0  
**Created**: 2026-02-05  
**Status**: 仕様確定
