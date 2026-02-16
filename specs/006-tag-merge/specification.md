# Phase 6: Tag Merge - 仕様書

## 1. 機能概要

### 1.1 目的
タグマージ機能は、複数のタグを1つのタグに統合する機能です。これにより、重複したタグや類似したタグを整理し、タグ管理を効率化できます。

### 1.2 主な機能
1. **既存タグへのマージ**: 複数のソースタグを既存のターゲットタグにマージ
2. **新規タグへのマージ**: 複数のソースタグを新しく作成したタグにマージ
3. **自動的な関連データの移行**: TODOタグとラベルタグが自動的に移行される
4. **マージ済みタグの非表示**: マージされたタグは一覧から自動的に除外される

### 1.3 ユースケース

#### UC1: 重複タグの統合
**シナリオ**: ユーザーが「JavaScript」と「JS」という2つのタグを作成してしまった場合

1. タグ一覧ページで「タグをマージ」ボタンをクリック
2. ソースタグとして「JS」を選択
3. 「既存のタグにマージ」を選択し、ターゲットタグとして「JavaScript」を選択
4. 「マージを実行」ボタンをクリック
5. 確認ダイアログで「実行」をクリック
6. 「JS」タグが付いていたTODOとラベルが「JavaScript」タグに移行される
7. 「JS」タグは一覧から消える

#### UC2: 複数タグの整理
**シナリオ**: 「frontend」「front-end」「FE」という3つのタグを「Frontend」に統一したい場合

1. タグマージページを開く
2. ソースタグとして「frontend」「front-end」「FE」を選択
3. 「新しいタグを作成してマージ」を選択
4. 新しいタグ名に「Frontend」を入力
5. カラーを選択
6. 「マージを実行」ボタンをクリック
7. 確認後、3つのタグが新しい「Frontend」タグに統合される

---

## 2. データモデル

### 2.1 データベーススキーマ

#### tagsテーブル
```sql
CREATE TABLE tags (
  id INT AUTO_INCREMENT PRIMARY KEY,
  ulid VARCHAR(26) NOT NULL UNIQUE,
  owner_id INT NOT NULL,
  name VARCHAR(50) NOT NULL,
  color VARCHAR(7),
  merged_to_ulid VARCHAR(26),  -- マージ先タグのULID
  merged_at DATETIME,           -- マージ日時
  created_at DATETIME NOT NULL,
  updated_at DATETIME NOT NULL,
  FOREIGN KEY (owner_id) REFERENCES users(id) ON DELETE CASCADE,
  INDEX idx_tags_owner_id (owner_id),
  INDEX idx_tags_ulid (ulid),
  INDEX idx_tags_merged_at (merged_at)
);
```

#### labelsテーブル
```sql
CREATE TABLE labels (
  id INT AUTO_INCREMENT PRIMARY KEY,
  ulid VARCHAR(26) NOT NULL UNIQUE,
  owner_id INT NOT NULL,
  name VARCHAR(100) NOT NULL,
  description TEXT,
  merged_to_ulid VARCHAR(26),  -- マージ先ラベルのULID
  merged_at DATETIME,           -- マージ日時
  created_at DATETIME NOT NULL,
  updated_at DATETIME NOT NULL,
  FOREIGN KEY (owner_id) REFERENCES users(id) ON DELETE CASCADE,
  INDEX idx_labels_owner_id (owner_id),
  INDEX idx_labels_ulid (ulid),
  INDEX idx_labels_merged_at (merged_at)
);
```

### 2.2 データフロー

#### マージ処理のフロー
```
1. バリデーション
   ├─ ソースタグの存在確認
   ├─ ターゲットタグの存在確認（既存タグへのマージの場合）
   ├─ 所有者チェック
   └─ ソースタグとターゲットタグが同一でないことの確認

2. トランザクション開始

3. 各ソースタグについて
   ├─ TODOタグの移行
   │  ├─ ソースタグのTODOタグを取得
   │  ├─ ターゲットタグに既にある場合はスキップ
   │  └─ 新しいTODOタグレコードを作成
   │
   ├─ ラベルタグの移行
   │  ├─ ソースタグのラベルタグを取得
   │  ├─ ターゲットタグに既にある場合はスキップ
   │  └─ 新しいラベルタグレコードを作成
   │
   ├─ 古いTODOタグレコードの削除
   ├─ 古いラベルタグレコードの削除
   │
   └─ ソースタグのマージ情報更新
      ├─ merged_to_ulid = ターゲットタグULID
      └─ merged_at = 現在時刻

4. トランザクションコミット

5. 成功レスポンス返却
```

### 2.3 重要な制約
1. **マージは不可逆**: 一度マージしたタグは元に戻せない
2. **重複防止**: 同じTODO/ラベルに同じタグが複数付かないようチェック
3. **所有者制限**: 自分が所有するタグのみマージ可能
4. **マージチェーン**: マージチェーンは最大10階層まで追跡可能

---

## 3. API仕様

### 3.1 既存タグへのマージ

#### エンドポイント
```
POST /api/v1/tags/merge
```

#### 認証
必須（セッションベース）

#### リクエスト
**Content-Type**: `application/json`

```json
{
  "source_ulids": ["01KHFFS79PW4SBJK3MGAQ8EW9C", "01KHFFV3VSRM9YPC4AVSK6K7YY"],
  "target_ulid": "01KHFG2HP5E8N9QXJC5W7VKTM3"
}
```

**パラメータ**:
- `source_ulids` (required): マージ元タグのULID配列
- `target_ulid` (required): マージ先タグのULID

#### レスポンス

**成功時 (200 OK)**:
```json
{
  "status": "success",
  "data": {
    "mergedTags": [
      {
        "ulid": "01KHFFS79PW4SBJK3MGAQ8EW9C",
        "name": "old-tag-1",
        "color": "#3B82F6",
        "mergedToUlid": "01KHFG2HP5E8N9QXJC5W7VKTM3",
        "mergedAt": "2026-02-16T12:00:00Z"
      }
    ],
    "targetTag": {
      "ulid": "01KHFG2HP5E8N9QXJC5W7VKTM3",
      "name": "target-tag",
      "color": "#10B981",
      "todoCount": 15,
      "completedCount": 5,
      "activeCount": 10,
      "createdAt": "2026-01-15T10:00:00Z",
      "updatedAt": "2026-02-16T12:00:00Z"
    }
  }
}
```

**エラー時**:

400 Bad Request:
```json
{
  "status": "error",
  "message": "source_ulids is required"
}
```

401 Unauthorized:
```json
{
  "status": "error",
  "message": "Authentication required"
}
```

404 Not Found:
```json
{
  "status": "error",
  "message": "Tag not found or not accessible"
}
```

### 3.2 新規タグへのマージ

#### エンドポイント
```
POST /api/v1/tags/merge-to-new
```

#### 認証
必須（セッションベース）

#### リクエスト
**Content-Type**: `application/json`

```json
{
  "source_ulids": ["01KHFFS79PW4SBJK3MGAQ8EW9C", "01KHFFV3VSRM9YPC4AVSK6K7YY"],
  "new_tag": {
    "name": "new-tag-name",
    "color": "#F59E0B"
  }
}
```

**パラメータ**:
- `source_ulids` (required): マージ元タグのULID配列
- `new_tag` (required): 新しいタグ情報
  - `name` (required): タグ名（1-50文字）
  - `color` (optional): カラーコード（デフォルト: `#3B82F6`）

#### レスポンス

**成功時 (200 OK)**:
```json
{
  "status": "success",
  "data": {
    "mergedTags": [
      {
        "ulid": "01KHFFS79PW4SBJK3MGAQ8EW9C",
        "name": "old-tag-1",
        "color": "#3B82F6",
        "mergedToUlid": "01KHFG9ZT4K3N8MXJC5W7VKTM9",
        "mergedAt": "2026-02-16T12:00:00Z"
      }
    ],
    "newTag": {
      "ulid": "01KHFG9ZT4K3N8MXJC5W7VKTM9",
      "name": "new-tag-name",
      "color": "#F59E0B",
      "todoCount": 10,
      "completedCount": 3,
      "activeCount": 7,
      "createdAt": "2026-02-16T12:00:00Z",
      "updatedAt": "2026-02-16T12:00:00Z"
    }
  }
}
```

**エラー時**:

400 Bad Request:
```json
{
  "status": "error",
  "message": "Tag name is required"
}
```

401 Unauthorized:
```json
{
  "status": "error",
  "message": "Authentication required"
}
```

---

## 4. 画面設計

### 4.1 タグマージページ

#### レイアウト
```
┌────────────────────────────────────────┐
│ Header                                  │
├────────────────────────────────────────┤
│                                         │
│  タグをマージ                            │
│                                         │
│  ┌──────────────────────────────────┐  │
│  │ ソースタグを選択                   │  │
│  │                                   │  │
│  │ □ JavaScript  □ TypeScript        │  │
│  │ □ React       □ Vue.js            │  │
│  │                                   │  │
│  └──────────────────────────────────┘  │
│                                         │
│  ┌──────────────────────────────────┐  │
│  │ マージ先を選択                     │  │
│  │                                   │  │
│  │ ○ 既存のタグにマージ              │  │
│  │   ▼ Frontend                      │  │
│  │                                   │  │
│  │ ○ 新しいタグを作成してマージ       │  │
│  │   [タグ名入力]                    │  │
│  │   🎨 カラー選択                   │  │
│  │                                   │  │
│  └──────────────────────────────────┘  │
│                                         │
│  [キャンセル]  [マージを実行]           │
│                                         │
└────────────────────────────────────────┘
```

#### UI要素

**ソースタグ選択エリア**:
- タグカード（チェックボックス付き）
- タグ名とカラーインジケーター
- 複数選択可能
- div全体がクリック可能

**マージ先選択エリア**:
- ラジオボタンで「既存」か「新規」を選択
- 既存の場合: ドロップダウンでタグ選択
- 新規の場合: 
  - テキスト入力フィールド（タグ名）
  - カラーピッカー

**アクションボタン**:
- キャンセル: タグ一覧に戻る
- マージを実行: 確認ダイアログを表示

#### 確認ダイアログ
```
┌─────────────────────────────────┐
│  タグのマージ                    │
├─────────────────────────────────┤
│                                  │
│  以下のタグをマージしますか？    │
│                                  │
│  ソースタグ:                     │
│  • JavaScript                    │
│  • TypeScript                    │
│                                  │
│  マージ先: Frontend              │
│                                  │
│  ⚠️ この操作は取り消せません    │
│                                  │
│  [キャンセル]  [実行]            │
│                                  │
└─────────────────────────────────┘
```

### 4.2 レスポンシブデザイン

#### モバイル（375px以上）
- 1カラムレイアウト
- タグカードは縦に並ぶ
- ボタンは全幅

#### タブレット（768px以上）
- 2カラムレイアウト（可能な範囲で）
- タグカードはグリッド表示

#### デスクトップ（1024px以上）
- 2カラムレイアウト
- タグカードは3-4カラムのグリッド

### 4.3 ダークモード

#### カラースキーム
- **ライトモード**:
  - 背景: `#ffffff`
  - テキスト: `#1f2937`
  - ボーダー: `#e5e7eb`
  
- **ダークモード**:
  - 背景: `#1f2937`
  - テキスト: `#f9fafb`
  - ボーダー: `#374151`

#### 自動切り替え
`prefers-color-scheme: dark`メディアクエリを使用して、デバイスの設定に応じて自動的に切り替わります。

---

## 5. セキュリティ

### 5.1 認証・認可
- すべてのAPIエンドポイントは認証必須
- セッションベースの認証
- 自分が所有するタグのみ操作可能

### 5.2 バリデーション
- **サーバーサイド**:
  - タグの存在確認
  - 所有者確認
  - タグ名の長さチェック（1-50文字）
  - カラーコード形式チェック
  
- **クライアントサイド**:
  - 必須項目チェック
  - タグ名の長さチェック
  - 入力値のサニタイゼーション

### 5.3 データ整合性
- トランザクションによる原子性保証
- 重複レコードの防止
- 外部キー制約による参照整合性

---

## 6. パフォーマンス

### 6.1 最適化手法
1. **クエリ最適化**:
   - インデックスの使用（owner_id, merged_at）
   - query + make-recordパターン
   - 必要最小限のデータ取得

2. **トランザクション管理**:
   - 適切なトランザクション境界
   - デッドロック回避

3. **フロントエンド**:
   - 状態管理の最適化
   - 不要な再レンダリングの防止

### 6.2 期待されるパフォーマンス
- タグマージAPI: < 500ms（10個のソースタグ、100個のTODO）
- ページ読み込み: < 1s
- UI応答性: < 100ms

---

## 7. エラーハンドリング

### 7.1 エラーケース

| エラー | HTTPステータス | メッセージ | 対処方法 |
|--------|---------------|-----------|---------|
| 認証エラー | 401 | Authentication required | ログインページにリダイレクト |
| タグ未選択 | 400 | source_ulids is required | エラーメッセージ表示 |
| マージ先未選択 | 400 | target_ulid is required | エラーメッセージ表示 |
| タグ名未入力 | 400 | Tag name is required | エラーメッセージ表示 |
| タグ名長すぎ | 400 | Tag name must be 50 characters or less | エラーメッセージ表示 |
| タグ不在 | 404 | Tag not found or not accessible | エラーメッセージ表示 |
| DBエラー | 500 | Internal server error | エラーメッセージ表示、ログ記録 |

### 7.2 ユーザーフィードバック
- エラーメッセージは明確で具体的
- 赤色で目立つように表示
- 解決方法のヒントを含める

---

## 8. テスト戦略

### 8.1 単体テスト
- モデル層の各関数
- サービス層の各関数
- バリデーション関数

### 8.2 統合テスト
- APIエンドポイント
- データベーストランザクション
- 認証・認可

### 8.3 E2Eテスト
- 既存タグへのマージフロー
- 新規タグへのマージフロー
- エラーケース
- レスポンシブデザイン

---

## 9. 制限事項

### 9.1 現在の制限
1. **不可逆操作**: マージは元に戻せない
2. **マージチェーン深さ**: 最大10階層
3. **バッチ処理なし**: 一度に1つのマージ操作のみ

### 9.2 将来的な拡張
1. マージ履歴の表示
2. マージの取り消し機能（一定期間内）
3. バッチマージ機能
4. マージプレビュー機能
5. マージの統計情報

---

## 10. 変更履歴

| 日付 | バージョン | 変更内容 | 担当者 |
|------|-----------|---------|--------|
| 2026-02-16 | 1.0.0 | 初版作成 | AI Assistant |

---

## 11. 参考資料

- [tasks.md](./tasks.md) - 実装タスクリスト
- [checklist.md](./checklist.md) - 実装チェックリスト
- [API Documentation](./api-documentation.md) - 詳細なAPI仕様
- [Database Schema](./database-schema.md) - データベーススキーマ詳細
