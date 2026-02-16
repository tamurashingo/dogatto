# Service層リファクタリング計画書

## 概要

DOGATTO プロジェクトにおけるService層の導入とリファクタリング計画。

**作成日**: 2026-02-11  
**ステータス**: Phase 5 実装中

---

## Service層の方針

### 基本原則

**Service層 = 業務ロジック層**

```
Controller → Service → Model
         ↑
    必ずこの流れで実装
```

### なぜService層を導入するのか

1. **将来の拡張性**
   - 単純なCRUDでも、将来の仕様変更に対応しやすくする
   - 検索条件の追加、バリデーション強化などが容易

2. **ビジネスロジックの集約**
   - Controllerは HTTP 処理のみ
   - Modelは データアクセスのみ
   - Serviceに業務ロジックを集中

3. **テストの容易さ**
   - Service層単体でテスト可能
   - HTTP処理とビジネスロジックを分離

---

## Model層とService層の棲み分け

### ✅ Model層の責務

**単一責任**: 1つのモデル（テーブル）に関する操作のみ

```lisp
;; ✅ Model層に実装
(defun find-tag-by-ulid (ulid)
  "単純な1件取得")

(defun find-tags-by-user (owner-id)
  "ユーザーのタグ一覧取得")

(defun create-tag (owner-id name &key color)
  "単一タグの作成")

(defun validate-tag (tag)
  "タグ単体の属性検証")
```

### ✅ Service層の責務

**統合・調整**: 複数Modelを組み合わせ、業務ロジックを実装

```lisp
;; ✅ Service層に実装
(defun list-tags (user-id &key sort order)
  "タグ一覧取得のビジネスロジック
   - 現在: 基本的な一覧
   - 将来: 使用頻度順ソート
   - 将来: 未使用タグフィルター")

(defun merge-tags-to-existing (source-ulids target-ulid owner-id)
  "複数タグのマージ
   - tags テーブル更新（マージ元）
   - todo_tags テーブル更新
   - label_tags テーブル更新
   - トランザクション管理")
```

### 判断フローチャート

```
処理を実装する場所を決める
  ↓
[Q1] 複数のテーブルを操作するか？
  YES → Service層
  NO → ↓
  
[Q2] トランザクション管理が必要か？
  YES → Service層
  NO → ↓
  
[Q3] 複数レコードの状態を総合判断するか？
  YES → Service層
  NO → ↓
  
[Q4] 外部システムと連携するか？
  YES → Service層
  NO → ↓
  
[Q5] 単純なCRUDだが、将来拡張の可能性があるか？
  YES → Service層（薄いラッパーでもOK）
  NO → Model層
```

---

## リファクタリング実施順序

### 🎯 Phase 5: タグマージ機能（新規実装）★ 最優先

**ステータス**: 実装中  
**期間**: 2026-02-09 ～ 2026-02-15（予定）

#### 実装内容

1. **✅ マイグレーション**
   - `db/migrate/20260209114321_add-merged-at-to-tags-and-labels.lisp`
   - merged_at カラム追加

2. **✅ Service層**
   - `app/services/tag-merge-service.lisp`
   - `validate-merge-sources` - マージ元検証
   - `validate-merge-target` - マージ先検証
   - `merge-tags-to-existing` - 既存タグへマージ
   - `merge-tags-to-new` - 新規タグへマージ
   - `resolve-merged-tag` - マージ解決

3. **TODO: Controller層**
   - `app/controllers/tags-merge-controller.lisp`
   - `POST /api/v1/tags/merge` - 既存タグへマージ
   - `POST /api/v1/tags/merge-to-new` - 新規タグへマージ

4. **TODO: ルーティング追加**
   - `app/config/environment.lisp`

5. **TODO: フロントエンド**
   - タグマージUI

#### 将来の拡張予定
- マージプレビュー機能
- マージ履歴表示
- マージの取り消し（一定期間内）
- 類似タグの自動提案

---

### Phase 3: タグ機能リファクタ ★ 次に実施

**ステータス**: 未着手  
**期間**: Phase 5 完了後  
**優先度**: 高（マージ機能と関連が深い）

#### 現状の問題点

```lisp
;; app/controllers/tags-controller.lisp
;; ❌ 直接Model呼び出し
(defmethod do-get ((controller <tags-list-controller>))
  (find-tags-by-user user-id))
```

#### リファクタ後

**新規作成: `app/services/tag-service.lisp`**

```lisp
(defun list-tags (user-id &key sort order)
  "タグ一覧取得のビジネスロジック
   - 現在: 基本的な一覧
   - 将来: 使用頻度順ソート
   - 将来: 未使用タグフィルター
   - 将来: マージ済みタグの扱い")

(defun get-tag-detail (tag-ulid user-id &key resolve-merge)
  "タグ詳細取得のビジネスロジック
   - 現在: タグ情報 + 統計
   - 将来: マージ解決 ★重要
   - 将来: 関連タグ提案")

(defun create-tag (user-id name color)
  "タグ作成のビジネスロジック
   - 現在: 基本的な作成
   - 将来: 重複チェック強化
   - 将来: 色の自動提案")

(defun update-tag (tag-ulid user-id &key name color)
  "タグ更新のビジネスロジック
   - 現在: 基本的な更新
   - 将来: 名前変更時の影響確認")

(defun delete-tag (tag-ulid user-id)
  "タグ削除のビジネスロジック
   - 現在: 論理削除
   - 将来: 使用中チェック
   - 将来: マージ提案（使用中の場合）")

(defun assign-tag-to-todo (todo-ulid tag-ulid user-id)
  "TODOにタグ割当のビジネスロジック
   - 現在: 単純な紐付け
   - 将来: 最大10個チェック
   - 将来: 関連タグ提案")

(defun remove-tag-from-todo (todo-ulid tag-ulid user-id)
  "TODOからタグ削除のビジネスロジック")
```

**修正: `app/controllers/tags-controller.lisp`**

```lisp
;; ✅ Service経由に変更
(defmethod do-get ((controller <tags-list-controller>))
  (tag-service:list-tags user-id))
```

#### 重要な変更点

- `get-tag-detail` にマージ解決機能を追加
  - マージされたタグへのアクセス時、最終的なタグを返す
  - `resolve-merge=true` パラメータで制御

---

### Phase 4: ラベル機能リファクタ

**ステータス**: 未着手  
**期間**: Phase 3 完了後  
**優先度**: 高（タグに依存）

#### 現状の問題点

```lisp
;; app/controllers/labels-controller.lisp
;; ❌ 直接Model呼び出し
(defmethod do-get ((controller <labels-list-controller>))
  (find-labels-by-user user-id))
```

#### リファクタ後

**新規作成: `app/services/label-service.lisp`**

```lisp
(defun list-labels (user-id &key search-mode query filter sort order)
  "ラベル一覧取得のビジネスロジック
   - 現在: 検索・フィルター・ソート
   - 将来: お気に入りラベル
   - 将来: ラベルグループ
   - 将来: マージ済みラベルの扱い")

(defun get-label-detail (label-ulid user-id &key resolve-merge)
  "ラベル詳細取得のビジネスロジック
   - 現在: ラベル情報 + タグ + 統計
   - 将来: マージ解決
   - 将来: 関連ラベル提案")

(defun create-label (user-id name description tag-ulids)
  "ラベル作成のビジネスロジック
   - 現在: 基本的な作成 + タグ関連付け
   - 将来: タグの妥当性検証強化
   - 将来: ラベル名の提案")

(defun update-label (label-ulid user-id &key name description tag-ulids)
  "ラベル更新のビジネスロジック
   - 現在: 基本的な更新 + タグ更新
   - 将来: タグ変更の影響確認
   - 将来: 変更履歴記録")

(defun delete-label (label-ulid user-id)
  "ラベル削除のビジネスロジック
   - 現在: 削除
   - 将来: 使用中チェック
   - 将来: マージ提案")

(defun estimate-todo-count (tag-ulids user-id)
  "TODO数推定のビジネスロジック
   - 現在: AND条件での推定
   - 将来: キャッシュ活用
   - 将来: 推定精度向上")
```

**修正: `app/controllers/labels-controller.lisp`**

```lisp
;; ✅ Service経由に変更
(defmethod do-get ((controller <labels-list-controller>))
  (label-service:list-labels user-id))
```

#### 追加実装

**新規作成: `app/services/label-merge-service.lisp`**

- ラベルマージ機能（Phase 5の後続）
- タグマージと同様の実装

---

### Phase 2: TODO機能リファクタ

**ステータス**: 未着手  
**期間**: Phase 4 完了後  
**優先度**: 中（比較的独立している）

#### 現状の問題点

```lisp
;; app/controllers/todos-controller.lisp
;; ❌ 直接Model呼び出し
(defmethod do-get ((controller <todos-list-controller>))
  (find-todos-by-user user-id))
```

#### リファクタ後

**新規作成: `app/services/todo-service.lisp`**

```lisp
(defun list-todos (user-id &key status tags label-ulid untagged sort order)
  "TODO一覧取得のビジネスロジック
   - 現在: 基本的な検索（ステータス、タグ、ラベル）
   - 将来: 優先度フィルター追加
   - 将来: 全文検索追加
   - 将来: 期限切れフィルター
   - 将来: 複雑なソート条件")

(defun get-todo-detail (todo-ulid user-id)
  "TODO詳細取得のビジネスロジック
   - 現在: TODO情報 + タグ
   - 将来: コメント取得
   - 将来: 履歴取得")

(defun create-todo (user-id title content &key due-date tags)
  "TODO作成のビジネスロジック
   - 現在: 基本的な作成 + タグ関連付け
   - 将来: 期限の自動設定（繰り返しタスク）
   - 将来: タグの自動提案
   - 将来: テンプレート適用")

(defun update-todo (todo-ulid user-id &key title content status due-date)
  "TODO更新のビジネスロジック
   - 現在: 基本的な更新
   - 将来: 履歴記録
   - 将来: 通知送信（期限変更時など）")

(defun delete-todo (todo-ulid user-id)
  "TODO削除のビジネスロジック
   - 現在: 削除
   - 将来: 論理削除への変更
   - 将来: 削除確認
   - 将来: アーカイブ機能")

(defun toggle-todo-status (todo-ulid user-id)
  "TODOステータス切替のビジネスロジック
   - 現在: pending ⇔ completed
   - 将来: ステータス遷移制御（pending → in-progress → completed）
   - 将来: 完了時の自動処理（統計更新など）")
```

**修正: `app/controllers/todos-controller.lisp`**

```lisp
;; ✅ Service経由に変更
(defmethod do-get ((controller <todos-list-controller>))
  (todo-service:list-todos user-id :status status :tags tags))
```

---

### Phase 1: 認証機能リファクタ

**ステータス**: 未着手  
**期間**: Phase 2 完了後（または後回し）  
**優先度**: 低（動作が安定している）

#### 現状の問題点

```lisp
;; app/controllers/auth-controller.lisp
;; ❌ 直接Model呼び出し
(defmethod do-post ((controller <auth-login-controller>))
  (let ((user (find-user-by-email email)))
    ...))
```

#### リファクタ後

**新規作成: `app/services/auth-service.lisp`**

```lisp
(defun authenticate-user (email password)
  "ユーザー認証のビジネスロジック
   - 現在: メール + パスワード認証
   - 将来: パスワードポリシー変更
   - 将来: 多要素認証追加
   - 将来: ログイン履歴記録
   - 将来: ログイン失敗回数制限")

(defun register-user (username email password)
  "ユーザー登録のビジネスロジック
   - 現在: 基本的な登録
   - 将来: メール認証
   - 将来: 招待制対応")

(defun get-current-user (session-id)
  "現在のユーザー取得のビジネスロジック
   - 現在: セッション確認
   - 将来: セッション更新
   - 将来: アクティビティ記録")
```

**修正: `app/controllers/auth-controller.lisp`**

```lisp
;; ✅ Service経由に変更
(defmethod do-post ((controller <auth-login-controller>))
  (auth-service:authenticate-user email password))
```

---

## 実装時の注意事項

### 1. トランザクション管理

Service層でトランザクションを管理する：

```lisp
(defun merge-tags-to-existing (source-ulids target-ulid owner-id)
  ;; ✅ Service層でトランザクション管理
  (with-transaction
    ;; 複数のModel操作
    ...))
```

### 2. エラーハンドリング

Service層でビジネスロジックのエラーをハンドリング：

```lisp
(defun create-tag (user-id name color)
  ;; バリデーション
  (when (tag-name-exists-p user-id name)
    (error 'validation-error :message "Tag name already exists"))
  
  ;; Model呼び出し
  (dogatto/models/tag:create-tag user-id name :color color))
```

### 3. 戻り値の形式

Service層の戻り値は統一：

```lisp
;; ✅ 成功時
(list :success t :data result)

;; ✅ 失敗時
(list :success nil :errors error-list)
```

### 4. パッケージ構成

```lisp
;; Service層
(defpackage #:dogatto/services/tag-service
  (:use #:cl)
  (:import-from #:dogatto/models/tag ...)
  (:export #:list-tags
           #:get-tag-detail
           #:create-tag
           ...))

;; Controller層
(defpackage #:dogatto/controllers/tags-controller
  (:use #:cl)
  (:import-from #:dogatto/services/tag-service
                #:list-tags
                #:get-tag-detail)
  ...)
```

---

## 進捗管理

### マイルストーン

| Phase | 機能 | 期間（予定） | ステータス |
|-------|------|------------|----------|
| Phase 5 | タグマージ | 2026-02-09 ～ 02-15 | 🟡 実装中 |
| Phase 3 | タグリファクタ | 02-16 ～ 02-20 | ⚪ 未着手 |
| Phase 4 | ラベルリファクタ | 02-21 ～ 02-25 | ⚪ 未着手 |
| Phase 2 | TODOリファクタ | 02-26 ～ 03-05 | ⚪ 未着手 |
| Phase 1 | 認証リファクタ | TBD | ⚪ 未着手 |

### チェックリスト（Phase 5）

- [x] マイグレーション作成
- [x] マイグレーション実行
- [x] tag-merge-service.lisp 作成
- [ ] tags-merge-controller.lisp 作成
- [ ] ルーティング追加
- [ ] application-loader.lisp にインポート追加
- [ ] フロントエンド実装
- [ ] テスト実装
- [ ] ドキュメント更新

---

## 参考資料

- [タグマージ仕様書](../.specify/memory/tag-merge-spec.md)
- [ラベルマージ仕様書](../.specify/memory/label-merge-spec.md)
- [実装計画書](../.specify/memory/implementation-plan.md)
- [AGENTS.md](../AGENTS.md) - 開発方針

---

**Version**: 1.0.0  
**Created**: 2026-02-11  
**Last Updated**: 2026-02-11  
**Author**: Development Team
