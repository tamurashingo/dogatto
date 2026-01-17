# 003-todo-basic 仕様書

## 概要

dogatto（タグベースTODO管理アプリケーション）の基本的なTODO機能を実装します。
認証システム（Phase 2完了）を前提に、ユーザーがTODOを作成・管理できる機能を提供します。

## 目標

ユーザーが以下の操作を実行できるようにする：
- TODOの作成（タイトル、説明、期限）
- TODOの一覧表示
- TODOの詳細表示
- TODOの編集
- TODOの削除
- TODOの完了/未完了切り替え

## 前提条件

- Phase 1（初期セットアップ）完了
- Phase 2（認証システム）完了
- ユーザーは認証済みであること
- TODOはユーザーごとに管理される

## スコープ

### Phase 3に含まれる機能

✅ **CRUD機能**：
- TODOの作成
- TODOの読み取り（一覧・詳細）
- TODOの更新
- TODOの削除

✅ **基本属性**：
- タイトル（必須）
- 説明（任意）
- 期限（任意）
- 完了状態（boolean）
- 作成日時
- 更新日時

✅ **ユーザー紐付け**：
- TODOは作成したユーザーに紐付く
- 他のユーザーのTODOは見えない

✅ **基本UI**：
- TODO一覧ページ
- TODO作成フォーム
- TODO編集フォーム
- TODO詳細表示

### Phase 3に含まれない機能（後のPhaseで実装）

❌ タグ機能（Phase 4）
❌ 検索・フィルタリング（Phase 5）
❌ ソート機能（Phase 5）
❌ ページネーション（Phase 5）
❌ TODO共有機能（Phase 6以降）
❌ 添付ファイル（Phase 6以降）
❌ コメント機能（Phase 6以降）

## データモデル

### TODOテーブル

```sql
CREATE TABLE todos (
  id BIGINT PRIMARY KEY AUTO_INCREMENT,
  user_id BIGINT NOT NULL,
  title VARCHAR(255) NOT NULL,
  description TEXT,
  due_date DATE,
  completed BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
  INDEX idx_user_id (user_id),
  INDEX idx_completed (completed),
  INDEX idx_due_date (due_date)
);
```

### バリデーション

- `title`: 必須、1-255文字
- `description`: 任意、65535文字まで
- `due_date`: 任意、有効な日付形式（YYYY-MM-DD）
- `completed`: boolean、デフォルトfalse

## API設計

### エンドポイント

| Method | Endpoint | 説明 | 認証 |
|--------|----------|------|------|
| GET | /api/v1/todos | TODO一覧取得 | 必須 |
| POST | /api/v1/todos | TODO作成 | 必須 |
| GET | /api/v1/todos/:id | TODO詳細取得 | 必須 |
| PUT | /api/v1/todos/:id | TODO更新 | 必須 |
| DELETE | /api/v1/todos/:id | TODO削除 | 必須 |
| PATCH | /api/v1/todos/:id/complete | TODO完了切替 | 必須 |

### レスポンス形式

**成功時**：
```json
{
  "status": "success",
  "data": {
    "todo": {
      "id": 1,
      "userId": 123,
      "title": "買い物",
      "description": "牛乳を買う",
      "dueDate": "2026-01-20",
      "completed": false,
      "createdAt": 1234567890,
      "updatedAt": 1234567890
    }
  }
}
```

**一覧取得時**：
```json
{
  "status": "success",
  "data": {
    "todos": [
      { /* todo object */ },
      { /* todo object */ }
    ],
    "count": 2
  }
}
```

**エラー時**：
```json
{
  "status": "error",
  "message": "エラーメッセージ"
}
```

## UI設計

### ページ構成

1. **TODO一覧ページ** (`/todos`)
   - ヘッダー（ユーザー情報、ログアウト）
   - TODO作成ボタン
   - TODO一覧（カード形式）
   - 各TODOに編集・削除ボタン

2. **TODO作成ページ** (`/todos/new`)
   - タイトル入力
   - 説明入力（テキストエリア）
   - 期限選択（日付ピッカー）
   - 保存・キャンセルボタン

3. **TODO編集ページ** (`/todos/:id/edit`)
   - 作成ページと同じフォーム
   - 既存データで初期化

4. **TODO詳細ページ** (`/todos/:id`)
   - TODO情報表示
   - 完了/未完了切替ボタン
   - 編集・削除ボタン

### UI要件

- レスポンシブデザイン（既存のauth UIと統一）
- ローディング状態の表示
- エラーメッセージの表示
- 完了したTODOは視覚的に区別（取り消し線など）
- 期限切れTODOの強調表示

## 技術要件

### バックエンド

- Common Lisp (SBCL)
- clailsフレームワーク
- MySQL（todos テーブル）
- 認証ミドルウェアによる保護

### フロントエンド

- React 18
- TypeScript
- React Router（ルーティング）
- 既存のAuthContextを利用
- 既存のapiClientを拡張

### テスト

- バックエンド：roveによるユニットテスト
- フロントエンド：Vitestによるユニットテスト
- 統合テスト：手動テスト

## 成功基準

✅ ユーザーがTODOを作成できる  
✅ ユーザーが自分のTODO一覧を表示できる  
✅ ユーザーがTODOを編集できる  
✅ ユーザーがTODOを削除できる  
✅ ユーザーがTODOを完了/未完了に切り替えられる  
✅ 他のユーザーのTODOは表示されない  
✅ すべてのテストが合格する  
✅ UIが既存デザインと統一されている  

## リスクと制約

### リスク
- 大量のTODOでパフォーマンス問題が発生する可能性
  → Phase 5でページネーション実装で対応

### 制約
- タグ機能なし（Phase 4で実装予定）
- 検索・フィルタなし（Phase 5で実装予定）
- 単純な一覧表示のみ

## 依存関係

- Phase 1: 完了済み ✅
- Phase 2: 完了済み ✅
- Phase 4以降: このPhase完了後に開始可能

---

**作成日**: 2026-01-17  
**ステータス**: Draft  
**次のステップ**: タスク分解（tasks.md作成）
