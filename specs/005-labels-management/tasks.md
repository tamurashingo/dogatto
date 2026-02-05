# 005-labels-management タスク管理

## 概要

Phase 5（ラベル管理機能）の実装タスクを管理します。

## 進捗サマリー

- **開始日**: 2026-02-05
- **目標完了日**: 2026-02-14（9日間）
- **ステータス**: 🟡 進行中

### 進捗状況

- **Phase 1 (バックエンド - データベース)**: 0/3 (0%)
- **Phase 2 (バックエンド - モデル)**: 0/4 (0%)
- **Phase 3 (バックエンド - コントローラー)**: 0/6 (0%)
- **Phase 4 (バックエンド - テスト)**: 0/3 (0%)
- **Phase 5 (フロントエンド - API)**: 0/2 (0%)
- **Phase 6 (フロントエンド - ページ)**: 0/4 (0%)
- **Phase 7 (フロントエンド - コンポーネント)**: 0/5 (0%)
- **Phase 8 (フロントエンド - スタイル)**: 0/3 (0%)
- **Phase 9 (フロントエンド - 統合)**: 0/3 (0%)
- **Phase 10 (ルーティング)**: 0/2 (0%)
- **Phase 11 (ドキュメント)**: 0/2 (0%)

**全体進捗**: 0/37 (0%)

---

## Phase 1: バックエンド - データベース

**目的**: ラベル関連のデータベーススキーマを作成

### T001 [P1] Labelsテーブルのマイグレーション作成
- [ ] マイグレーションファイル作成
  - `db/migrate/YYYYMMDDHHMMSS_create-labels-table.lisp`
  - labels テーブル定義
    - id (BIGINT, AUTO_INCREMENT, PRIMARY KEY)
    - ulid (VARCHAR(26), NOT NULL, UNIQUE)
    - owner-id (BIGINT, NOT NULL, FOREIGN KEY)
    - name (VARCHAR(100), NOT NULL)
    - description (TEXT)
    - created-at (BIGINT, NOT NULL)
    - updated-at (BIGINT, NOT NULL)
  - インデックス作成
    - owner_id
    - ulid (UNIQUE)
    - (owner_id, name) UNIQUE制約
- [ ] マイグレーション実行テスト
- [ ] ロールバックテスト

**推定時間**: 1時間

### T002 [P1] Label_Tagsテーブルのマイグレーション作成
- [ ] マイグレーションファイル作成
  - `db/migrate/YYYYMMDDHHMMSS_create-label-tags-table.lisp`
  - label_tags テーブル定義
    - id (BIGINT, AUTO_INCREMENT, PRIMARY KEY)
    - label-id (BIGINT, NOT NULL, FOREIGN KEY)
    - tag-id (BIGINT, NOT NULL, FOREIGN KEY)
    - created-at (BIGINT, NOT NULL)
  - インデックス作成
    - label_id
    - tag_id
    - (label_id, tag_id) UNIQUE制約
  - CASCADE削除設定
- [ ] マイグレーション実行テスト
- [ ] ロールバックテスト

**推定時間**: 1時間

### T003 [P1] シードデータ作成（開発用）
- [ ] テスト用ラベルデータ作成
  - 3〜5個のサンプルラベル
  - 各ラベルに2〜3個のタグを関連付け
- [ ] TODO-Label関連の確認データ

**推定時間**: 30分

**Phase 1 推定時間**: 2.5時間

---

## Phase 2: バックエンド - モデル

**目的**: Labelモデルと関連ロジックの実装

### T004 [P1] Labelモデルの実装
- [ ] モデルファイル作成
  - `app/models/label.lisp`
  - package定義
- [ ] Label クラス定義
  - スロット定義（id, ulid, owner-id, name, description, created-at, updated-at）
  - アクセサー定義
- [ ] CRUD関数の実装
  - find-label-by-ulid (ulid owner-id)
  - find-labels-by-owner (owner-id &key page per-page sort order filter search-mode q)
  - create-label (owner-id name description tag-ulids)
  - update-label (ulid owner-id name description tag-ulids)
  - delete-label (ulid owner-id)
- [ ] バリデーション関数
  - validate-label-name (name)
  - validate-label-description (description)
  - check-label-name-uniqueness (owner-id name &optional exclude-ulid)

**推定時間**: 3時間

### T005 [P1] Label-Tag関連付けの実装
- [ ] Label-Tagモデルファイル作成
  - `app/models/label-tag.lisp`
  - package定義
- [ ] Label-Tag クラス定義
- [ ] 関連付け関数
  - get-label-tags (label-id)
  - set-label-tags (label-id tag-ids)
  - add-label-tag (label-id tag-id)
  - remove-label-tag (label-id tag-id)
  - clear-label-tags (label-id)
- [ ] タグ取得関数
  - get-tags-for-label (label-ulid owner-id)

**推定時間**: 2時間

### T006 [P1] TODO数推定機能の実装
- [ ] TODO数推定関数
  - estimate-todo-count-by-tags (owner-id tag-ulids)
  - タグのAND条件でTODO数を計算
- [ ] ラベルのTODO数計算
  - get-label-todo-count (label-ulid owner-id)

**推定時間**: 2時間

### T007 [P1] ラベル検索機能の実装
- [ ] ラベル名検索
  - search-labels-by-name (owner-id query)
- [ ] タグ名検索
  - search-labels-by-tag-name (owner-id query)
- [ ] 統計情報取得
  - get-label-stats (owner-id)
  - total-labels, used-labels, unused-labels

**推定時間**: 2時間

**Phase 2 推定時間**: 9時間

---

## Phase 3: バックエンド - コントローラー

**目的**: ラベル管理のAPIエンドポイント実装

### T008 [P1] Labelsコントローラーの実装
- [ ] コントローラーファイル作成
  - `app/controllers/labels-controller.lisp`
  - package定義
  - use clails/controller/base-controller
- [ ] GET /api/v1/labels (ラベル一覧)
  - クエリパラメータ処理（page, per_page, sort, order, filter, search_mode, q）
  - ページネーション実装
  - ソート機能実装
  - フィルタ機能実装
  - 検索機能実装
  - レスポンス生成
- [ ] POST /api/v1/labels (ラベル作成)
  - リクエストボディ検証
  - バリデーション
  - ラベル作成
  - タグ関連付け
  - レスポンス生成（201 Created）
- [ ] GET /api/v1/labels/:ulid (ラベル詳細)
  - パラメータ検証
  - ラベル取得
  - 関連タグ取得
  - 関連TODO取得（AND条件）
  - レスポンス生成
- [ ] PUT /api/v1/labels/:ulid (ラベル更新)
  - パラメータ検証
  - バリデーション
  - ラベル更新
  - タグ関連付け更新
  - レスポンス生成
- [ ] DELETE /api/v1/labels/:ulid (ラベル削除)
  - パラメータ検証
  - ラベル削除（CASCADE）
  - レスポンス生成（204 No Content）

**推定時間**: 4時間

### T009 [P1] TODO数推定エンドポイントの実装
- [ ] GET /api/v1/labels/estimate-todo-count
  - クエリパラメータ検証（tag_ulids）
  - TODO数計算
  - レスポンス生成

**推定時間**: 1時間

### T010 [P1] ラベル使用状況エンドポイントの実装
- [ ] GET /api/v1/labels/:ulid/usage
  - パラメータ検証
  - ラベル取得
  - 関連TODO取得
  - 使用状況レスポンス生成

**推定時間**: 1時間

### T011 [P1] TODOフィルタリングの拡張
- [ ] TodosControllerの修正
  - `app/controllers/todos-controller.lisp`
  - GET /api/v1/todos にlabelパラメータ追加
  - ラベルによるフィルタリング実装（AND条件）
  - 既存のタグフィルタとの併用対応

**推定時間**: 1時間

### T012 [P1] 認可チェックの実装
- [ ] ラベル所有者チェック
  - check-label-ownership (label-ulid user-id)
- [ ] 各エンドポイントに認可チェック追加
  - GET /api/v1/labels/:ulid
  - PUT /api/v1/labels/:ulid
  - DELETE /api/v1/labels/:ulid

**推定時間**: 1時間

### T013 [P1] エラーハンドリングの実装
- [ ] ラベル専用エラー定義
  - label-not-found-error
  - label-name-duplicate-error
  - label-validation-error
- [ ] エラーレスポンス生成
  - 404: Label not found
  - 409: Label name already exists
  - 400: Validation error
  - 403: Access denied

**推定時間**: 1時間

**Phase 3 推定時間**: 9時間

---

## Phase 4: バックエンド - テスト

**目的**: バックエンド機能のテストコード作成

### T014 [P1] Labelモデルのテスト
- [ ] テストファイル作成
  - `test/models/label.lisp`
- [ ] CRUD操作のテスト
  - ラベル作成テスト
  - ラベル取得テスト
  - ラベル更新テスト
  - ラベル削除テスト
- [ ] バリデーションテスト
  - 名前バリデーション
  - 重複チェック
  - タグ必須チェック

**推定時間**: 2時間

### T015 [P1] Labelsコントローラーのテスト
- [ ] テストファイル作成
  - `test/controllers/labels-controller.lisp`
- [ ] APIエンドポイントのテスト
  - GET /api/v1/labels
  - POST /api/v1/labels
  - GET /api/v1/labels/:ulid
  - PUT /api/v1/labels/:ulid
  - DELETE /api/v1/labels/:ulid
- [ ] エラーケースのテスト
  - 404エラー
  - 409エラー
  - 400エラー
  - 403エラー

**推定時間**: 3時間

### T016 [P1] 統合テスト
- [ ] ラベル作成フローのテスト
- [ ] ラベル編集フローのテスト
- [ ] ラベル削除フローのテスト
- [ ] ラベルによるTODO検索テスト
- [ ] 認可テスト

**推定時間**: 2時間

**Phase 4 推定時間**: 7時間

---

## Phase 5: フロントエンド - API

**目的**: ラベルAPIクライアントの実装

### T017 [P1] ラベルAPIクライアントの実装
- [ ] APIクライアントファイル作成
  - `front/src/api/labels.ts`
- [ ] 型定義
  - Label型
  - LabelCreateParams型
  - LabelUpdateParams型
  - LabelsResponse型
  - LabelDetailResponse型
- [ ] API関数の実装
  - getLabels(params)
  - getLabel(ulid)
  - createLabel(params)
  - updateLabel(ulid, params)
  - deleteLabel(ulid)
  - estimateTodoCount(tagUlids)
  - getLabelUsage(ulid)
- [ ] エラーハンドリング

**推定時間**: 2時間

### T018 [P1] ラベルAPIクライアントのテスト
- [ ] テストファイル作成
  - `front/src/api/labels.test.ts`
- [ ] ユニットテスト
  - 各API関数のテスト
  - エラーハンドリングのテスト

**推定時間**: 1時間

**Phase 5 推定時間**: 3時間

---

## Phase 6: フロントエンド - ページ

**目的**: ラベル管理ページの実装

### T019 [P1] ラベル一覧ページの実装
- [ ] ページファイル作成
  - `front/src/pages/LabelsPage.tsx`
- [ ] ページコンポーネント実装
  - ラベル一覧取得
  - 検索機能（ラベル名/タグ名）
  - フィルター機能（全て/使用中/未使用）
  - ソート機能
  - ページネーション
  - 統計情報表示
- [ ] 空状態の実装
- [ ] ローディング状態
- [ ] エラー表示

**推定時間**: 3時間

### T020 [P2] ラベル詳細ページの実装（将来）
- [ ] ページファイル作成
  - `front/src/pages/LabelDetailPage.tsx`
- [ ] ラベル詳細表示
- [ ] 関連TODO一覧表示
- [ ] 編集・削除ボタン

**推定時間**: 2時間（Phase 6で実装予定）

### T021 [P1] 検索機能の実装
- [ ] 検索モード切り替え（ラベル名/タグ名）
- [ ] 検索入力フォーム
- [ ] Debounce処理（300ms）
- [ ] 検索結果のハイライト
- [ ] クリアボタン

**推定時間**: 1時間

### T022 [P1] フィルター・ソート機能の実装
- [ ] フィルタードロップダウン（全て/使用中/未使用）
- [ ] ソートドロップダウン
  - 名前（昇順/降順）
  - タグ数（昇順/降順）
  - TODO数（昇順/降順）
  - 更新日（昇順/降順）
- [ ] URLクエリパラメータ連携

**推定時間**: 1時間

**Phase 6 推定時間**: 7時間

---

## Phase 7: フロントエンド - コンポーネント

**目的**: ラベル関連のUIコンポーネント実装

### T023 [P1] ラベル作成モーダルの実装
- [ ] コンポーネントファイル作成
  - `front/src/components/LabelFormModal.tsx`
- [ ] フォームフィールド
  - 名前入力（必須、最大100文字）
  - 説明入力（任意、最大1000文字）
  - タグ選択ドロップダウン
- [ ] バリデーション
  - 名前バリデーション
  - タグ必須チェック
  - エラー表示
- [ ] プレビュー機能
  - 推定TODO数表示
  - リアルタイム計算（Debounce: 500ms）
- [ ] 送信・キャンセル処理

**推定時間**: 3時間

### T024 [P1] ラベル編集モーダルの実装
- [ ] 既存値の表示
- [ ] フォーム編集
- [ ] タグ変更時の確認ダイアログ
  - 変更前後のタグ表示
  - 変更前後のTODO数表示
  - 警告メッセージ
- [ ] 更新処理

**推定時間**: 2時間

### T025 [P1] タグドロップダウンコンポーネントの実装
- [ ] コンポーネントファイル作成
  - `front/src/components/TagDropdown.tsx`
- [ ] タグ一覧表示
  - タグ名
  - タグの色
  - TODO数
- [ ] タグ検索機能
- [ ] 選択済みタグの無効化
- [ ] キーボードナビゲーション（↑↓、Enter）
- [ ] 選択したタグの追加

**推定時間**: 2時間

### T026 [P1] ラベルカードコンポーネントの実装
- [ ] コンポーネントファイル作成
  - `front/src/components/LabelCard.tsx`
- [ ] ラベル情報表示
  - ラベル名
  - タグバッジ（最大5つ、それ以上は+N）
  - TODO数
  - 更新日
- [ ] 操作ボタン
  - 編集ボタン
  - 削除ボタン
- [ ] ホバー時のタグ全表示

**推定時間**: 2時間

### T027 [P1] 削除確認ダイアログの実装
- [ ] コンポーネントファイル作成
  - `front/src/components/LabelDeleteConfirm.tsx`
- [ ] 削除確認メッセージ
- [ ] 影響範囲表示（TODO数）
- [ ] 警告メッセージ
- [ ] 削除・キャンセルボタン

**推定時間**: 1時間

**Phase 7 推定時間**: 10時間

---

## Phase 8: フロントエンド - スタイル

**目的**: ラベル機能のスタイリング

### T028 [P1] ラベル一覧ページのスタイル
- [ ] CSSファイル作成
  - `front/src/styles/labels.css`
- [ ] ラベルカードのスタイル
- [ ] グリッドレイアウト
- [ ] レスポンシブデザイン
  - デスクトップ（1024px以上）
  - タブレット（768px-1023px）
  - モバイル（767px以下）

**推定時間**: 2時間

### T029 [P1] ラベルモーダルのスタイル
- [ ] CSSファイル作成
  - `front/src/styles/label-modal.css`
- [ ] モーダルのスタイル
- [ ] フォームのスタイル
- [ ] タグドロップダウンのスタイル
- [ ] レスポンシブ対応

**推定時間**: 2時間

### T030 [P1] ダークモード対応
- [ ] ダークモード用のカラースキーム
- [ ] メディアクエリ追加
- [ ] コントラスト確認

**推定時間**: 1時間

**Phase 8 推定時間**: 5時間

---

## Phase 9: フロントエンド - 統合

**目的**: ラベル機能の統合とテスト

### T031 [P1] ラベルフィルターの統合
- [ ] TodosPageにラベルフィルター追加
  - `front/src/pages/TodosPage.tsx`
- [ ] ラベルドロップダウン
- [ ] ラベルによるフィルタリング実装
- [ ] タグフィルターとの併用対応
- [ ] URLクエリパラメータ対応

**推定時間**: 2時間

### T032 [P1] E2Eテスト
- [ ] ラベル作成フローのテスト
- [ ] ラベル編集フローのテスト
- [ ] ラベル削除フローのテスト
- [ ] ラベルによるTODO検索テスト

**推定時間**: 2時間

### T033 [P1] バグ修正と最適化
- [ ] バグ修正
- [ ] パフォーマンス最適化
- [ ] ユーザビリティ改善

**推定時間**: 2時間

**Phase 9 推定時間**: 6時間

---

## Phase 10: ルーティング設定

**目的**: ラベルページのルーティング設定

### T034 [P1] ルート追加
- [ ] router.tsx修正
  - `/labels` → LabelsPage
  - `/labels/:ulid` → LabelDetailPage（将来実装）
- [ ] ProtectedRouteで保護

**推定時間**: 30分

### T035 [P1] ナビゲーションメニューの更新
- [ ] Headerにラベルリンク追加
  - `front/src/components/Header.tsx`
- [ ] アクティブ状態の表示

**推定時間**: 30分

**Phase 10 推定時間**: 1時間

---

## Phase 11: ドキュメント

**目的**: ドキュメントの更新

### T036 [P2] APIドキュメントの作成
- [ ] Labels API仕様書作成
  - `docs/labels-api.md`
- [ ] 全エンドポイントの記載
- [ ] リクエスト/レスポンス例
- [ ] エラーレスポンス

**推定時間**: 2時間

### T037 [P2] README更新
- [ ] ラベル機能の説明追加
- [ ] ドキュメントリンク追加

**推定時間**: 30分

**Phase 11 推定時間**: 2.5時間

---

## タスク優先度

### P1（必須）: コア機能
- データベースマイグレーション
- モデル・コントローラー実装
- ラベル一覧ページ
- ラベル作成/編集モーダル
- APIクライアント
- 基本的なスタイリング
- ルーティング設定

### P2（推奨）: 拡張機能
- ラベル詳細ページ
- 高度な検索機能
- 詳細なドキュメント
- ダークモード最適化

### P3（任意）: Nice-to-have
- アニメーション
- キーボードショートカット
- エクスポート機能

---

## マイルストーン

| マイルストーン | 期間 | 主要成果物 |
|--------------|------|-----------|
| M1: データベース完成 | Day 1 | マイグレーション、シードデータ |
| M2: バックエンドAPI完成 | Day 3 | モデル、コントローラー、テスト |
| M3: フロントエンド基盤完成 | Day 5 | APIクライアント、ページ骨格 |
| M4: UI完成 | Day 7 | コンポーネント、スタイリング |
| M5: 統合完了 | Day 9 | 統合、テスト、ドキュメント |

---

## リスク管理

### リスク1: タグとの複雑な関連
**影響度**: 中  
**対応策**: 
- 既存のタグ機能を参考に実装
- AND条件の実装に注意

### リスク2: TODO数計算のパフォーマンス
**影響度**: 中  
**対応策**:
- インデックス最適化
- Debounce処理
- キャッシング検討

### リスク3: UI/UXの複雑化
**影響度**: 低  
**対応策**:
- タグ機能のUIを参考に統一感を保つ
- ユーザーフィードバックを早期に取得

---

## 次のアクション

1. **Phase 1を開始**: データベースマイグレーション作成
2. **T001に着手**: Labelsテーブルのマイグレーション作成

---

**Version**: 1.0.0  
**Created**: 2026-02-05  
**Last Updated**: 2026-02-05  
**Status**: 🟡 進行中
