# 003-todo-basic タスクリスト

## 概要

TODO基本機能の実装タスクを定義します。Phase 2（認証システム）完了を前提とします。

---

## Phase 1: バックエンド - TODOモデル

**目的**: TODOデータモデルとデータベースマイグレーションを実装

- [x] T001 [P1] マイグレーションファイルを作成（create_todos_table）
  - id, user_id, title, description, due_date, completed, created_at, updated_at
  - user_idのFOREIGN KEY制約
  - 適切なINDEX追加
- [x] T002 [P1] clails generate:model todoでモデルを生成（--no-migration）
- [x] T003 [P1] app/models/todo.lispにバリデーションを実装
  - title必須、1-255文字
  - descriptionは任意
  - due_dateは任意、有効な日付形式
- [x] T004 [P1] TODOモデルのテストを作成
  - バリデーションテスト
  - CRUD操作テスト

---

## Phase 2: バックエンド - TODOコントローラー

**目的**: TODO操作のためのRESTful APIエンドポイントを実装

- [x] T005 [P1] clails generate:controller todosでコントローラーを生成
- [x] T006 [P1] GET /api/v1/todos エンドポイントを実装
  - 認証ユーザーのTODO一覧を返す
  - user_idでフィルタリング
  - JSON形式で返す
- [x] T007 [P1] POST /api/v1/todos エンドポイントを実装
  - リクエストボディからTODOを作成
  - バリデーション
  - 認証ユーザーのuser_idを自動設定
- [x] T008 [P1] GET /api/v1/todos/:id エンドポイントを実装
  - 指定IDのTODO詳細を返す
  - 自分のTODOのみアクセス可能（認可チェック）
- [x] T009 [P1] PUT /api/v1/todos/:id エンドポイントを実装
  - TODO更新
  - 認可チェック（自分のTODOのみ）
  - バリデーション
- [x] T010 [P1] DELETE /api/v1/todos/:id エンドポイントを実装
  - TODO削除
  - 認可チェック（自分のTODOのみ）
- [x] T011 [P1] PUT /api/v1/todos/:id/complete エンドポイントを実装
  - completedフラグの切り替え
  - 認可チェック
- [x] T012 [P1] TODOコントローラーのテストを作成
  - 各エンドポイントのテスト
  - 認可テスト（他ユーザーのTODOへのアクセス拒否）
  - バリデーションテスト

---

## Phase 3: バックエンド - ルーティング設定

**目的**: TODOエンドポイントをルーティングに追加

- [x] T013 [P1] app/config/environment.lispにTODOルートを追加
  - GET /api/v1/todos
  - POST /api/v1/todos
  - GET /api/v1/todos/:id
  - PUT /api/v1/todos/:id
  - DELETE /api/v1/todos/:id
  - PUT /api/v1/todos/:id/complete
- [x] T014 [P1] すべてのTODOルートに認証ミドルウェアを適用
- [x] T015 [P1] application-loaderにtodoモデルとコントローラーを追加

---

## Phase 4: フロントエンド - TODO API クライアント

**目的**: TODO操作のためのAPIクライアントを実装

- [x] T016 [P1] front/src/api/todos.tsを作成
- [x] T017 [P1] TypeScript型定義を作成
  - Todo型
  - CreateTodoRequest型
  - UpdateTodoRequest型
- [x] T018 [P1] getTodos関数を実装
- [x] T019 [P1] getTodoById関数を実装
- [x] T020 [P1] createTodo関数を実装
- [x] T021 [P1] updateTodo関数を実装
- [x] T022 [P1] deleteTodo関数を実装
- [x] T023 [P1] toggleTodoComplete関数を実装
- [x] T024 [P1] APIクライアントのテストを作成
  - 各関数のユニットテスト
  - エラーハンドリングテスト

---

## Phase 5: フロントエンド - TODO一覧ページ

**目的**: TODO一覧表示UIを実装

- [x] T025 [P1] front/src/pages/TodoListPage.tsxを作成
- [x] T026 [P1] TODO一覧の取得と表示
  - useEffect でTODO取得
  - ローディング状態
  - エラー状態
- [x] T027 [P1] TODOカードコンポーネントを作成
  - タイトル、説明、期限表示
  - 完了/未完了の視覚的区別
  - 編集・削除ボタン
- [x] T028 [P1] TODO作成ボタンを追加
  - /todos/new へのリンク
- [x] T029 [P1] TODO完了切り替え機能を実装
  - チェックボックスまたはボタン
  - 楽観的更新
- [x] T030 [P1] TODO削除機能を実装
  - 確認ダイアログ
  - 削除後の一覧更新
- [x] T031 [P1] 空状態の表示
  - TODOがない場合のメッセージ
  - 作成を促すCTA

---

## Phase 6: フロントエンド - TODO作成ページ

**目的**: TODO作成UIを実装

- [x] T032 [P1] front/src/pages/TodoCreatePage.tsxを作成
- [x] T033 [P1] TODO作成フォームを実装
  - タイトル入力（必須）
  - 説明入力（任意、textarea）
  - 期限選択（任意、日付ピッカー）
- [x] T034 [P1] フォームバリデーションを実装
  - タイトル必須チェック
  - 文字数制限チェック
- [x] T035 [P1] 作成APIを呼び出し
  - 成功時: TODO一覧へリダイレクト
  - エラー時: エラーメッセージ表示
- [x] T036 [P1] キャンセルボタンを実装
  - TODO一覧へ戻る

---

## Phase 7: フロントエンド - TODO編集ページ

**目的**: TODO編集UIを実装

- [ ] T037 [P1] front/src/pages/TodoEditPage.tsxを作成
- [ ] T038 [P1] 既存TODOデータの取得と表示
  - URLパラメータからID取得
  - getTodoByIdでデータ取得
  - フォームに初期値設定
- [ ] T039 [P1] 編集フォームを実装
  - 作成フォームと同じ構造
  - 既存データで初期化
- [ ] T040 [P1] 更新APIを呼び出し
  - 成功時: TODO一覧へリダイレクト
  - エラー時: エラーメッセージ表示
- [ ] T041 [P1] キャンセルボタンを実装

---

## Phase 8: フロントエンド - TODO詳細ページ

**目的**: TODO詳細表示UIを実装

- [ ] T042 [P1] front/src/pages/TodoDetailPage.tsxを作成
- [ ] T043 [P1] TODO詳細情報の取得と表示
  - タイトル、説明、期限、完了状態
  - 作成日時、更新日時
- [ ] T044 [P1] アクションボタンを実装
  - 編集ボタン → 編集ページへ
  - 削除ボタン → 確認後削除して一覧へ
  - 完了切り替えボタン
- [ ] T045 [P1] 戻るボタンを実装
  - TODO一覧へ戻る

---

## Phase 9: フロントエンド - ルーティング設定

**目的**: TODOページのルートを追加

- [ ] T046 [P1] router.tsxにTODOルートを追加
  - /todos → TodoListPage（保護ルート）
  - /todos/new → TodoCreatePage（保護ルート）
  - /todos/:id → TodoDetailPage（保護ルート）
  - /todos/:id/edit → TodoEditPage（保護ルート）
- [ ] T047 [P1] すべてのTODOルートをProtectedRouteで保護
- [ ] T048 [P1] ナビゲーションの更新
  - ヘッダーにTODO一覧へのリンク追加

---

## Phase 10: フロントエンド - スタイリング

**目的**: TODOページのUIを既存デザインと統一

- [ ] T049 [P2] front/src/styles/todos.cssを更新
  - TODO一覧のグリッドレイアウト
  - TODOカードのスタイル
  - フォームのスタイル
- [ ] T050 [P2] 完了TODOの視覚的区別
  - 取り消し線
  - 薄いグレー
- [ ] T051 [P2] 期限切れTODOの強調表示
  - 赤文字または背景色
- [ ] T052 [P2] レスポンシブデザイン
  - モバイル対応
  - タブレット対応
- [ ] T053 [P2] ダークモード対応
  - 既存デザインと統一

---

## Phase 11: 統合とテスト

**目的**: すべてのコンポーネントを統合しテスト

- [ ] T054 [P1] エンドツーエンドテスト
  - TODO作成フロー
  - TODO編集フロー
  - TODO削除フロー
  - TODO完了切り替え
- [ ] T055 [P1] 認可テスト
  - 他ユーザーのTODOへのアクセス拒否
  - 未認証ユーザーのアクセス拒否
- [ ] T056 [P1] エラーケースのテスト
  - 存在しないTODOへのアクセス
  - 無効なデータでの作成・更新
  - ネットワークエラー
- [ ] T057 [P1] 統合の問題を修正

---

## Phase 12: ドキュメント

**目的**: ドキュメントを更新

- [ ] T058 [P2] README.mdにTODO機能を追加
- [ ] T059 [P2] API documentationを更新（todos-api.md作成）
- [ ] T060 [P2] データベーススキーマドキュメントを更新
- [ ] T061 [P2] 未使用のコードを削除

---

## タスク優先度

**P1（必須）**: コア機能、なければ動作しない  
**P2（重要）**: UX向上、後で実装可能

## 依存関係

```
Phase 1 → Phase 2 → Phase 3
                     ↓
Phase 4 → Phase 5, 6, 7, 8 → Phase 9
                              ↓
                         Phase 10 → Phase 11 → Phase 12
```

## 推定工数

- Phase 1: 3時間
- Phase 2: 5時間
- Phase 3: 1時間
- Phase 4: 3時間
- Phase 5: 4時間
- Phase 6: 3時間
- Phase 7: 3時間
- Phase 8: 2時間
- Phase 9: 1時間
- Phase 10: 4時間
- Phase 11: 4時間
- Phase 12: 2時間

**合計**: 約35時間（1週間）

---

**作成日**: 2026-01-17  
**ステータス**: Ready for implementation  
**次のステップ**: Phase 1から実装開始
