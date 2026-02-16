# Phase 6: Tag Merge - タスクリスト

このドキュメントは Phase 6 (Tag Merge) の実装タスクを管理します。

## タスク優先度

- **P1（必須）**: コア機能、なければ動作しない
- **P2（推奨）**: ユーザー体験に大きく影響
- **P3（任意）**: あれば便利

---

## Phase 1: データベースとモデル

**目的**: タグマージのためのデータベーススキーマ変更とモデル実装

- [x] T001 [P1] マイグレーションファイル作成（merged_at追加）
  - tagsテーブルにmerged_atカラム追加（INTEGER型）
  - labelsテーブルにmerged_atカラム追加（INTEGER型）
  - インデックスの追加
- [x] T002 [P1] merged_at型をDATETIMEに変更
  - INTEGER型からDATETIME型に変更するマイグレーション
  - カラム削除→再作成で型変更
  - インデックス再作成
- [x] T003 [P1] Tag-Tag関連の取得関数
  - `find-tags-by-user`をマージ済みタグ除外に修正
  - マージ済みタグ（merged_at IS NOT NULL）を除外

---

## Phase 2: バックエンド - Tag Merge Service

**目的**: タグマージのビジネスロジックを実装

- [x] T004 [P1] tag-merge-serviceの作成
  - `validate-merge-sources` - ソースタグのバリデーション
  - `validate-merge-target` - ターゲットタグのバリデーション
  - `merge-tags-to-existing` - 既存タグへのマージ
  - `merge-tags-to-new` - 新規タグへのマージ
  - `resolve-merged-tag` - マージチェーンの解決
- [x] T005 [P1] todo-tagモデルにマージ用関数追加
  - `copy-todo-tags-for-merge` - TODOタグ関連のコピー
  - `delete-todo-tags-for-merge` - TODOタグ関連の削除
- [x] T006 [P1] label-tagモデルにマージ用関数追加
  - `copy-label-tags-for-merge` - ラベルタグ関連のコピー
  - `delete-label-tags-for-merge` - ラベルタグ関連の削除
- [x] T007 [P1] SQL最適化
  - SELECT-INSERTからquery+make-recordに変更
  - フレームワークにタイムスタンプ処理を任せる

---

## Phase 3: バックエンド - Tag Merge API エンドポイント

**目的**: タグマージ用のAPIエンドポイントを実装

- [x] T008 [P1] tags-merge-controllerの作成
  - `<tags-merge-controller>` - 既存タグへのマージ
  - `<tags-merge-to-new-controller>` - 新規タグへのマージ
- [x] T009 [P1] POST /api/v1/tags/merge
  - 既存タグへのマージ
  - バリデーション
  - 認証チェック
  - トランザクション処理
- [x] T010 [P1] POST /api/v1/tags/merge-to-new
  - 新規タグを作成してマージ
  - バリデーション
  - 認証チェック
  - トランザクション処理
- [x] T011 [P1] エラーハンドリング
  - バリデーションエラー
  - 認証エラー
  - DB エラー
- [x] T012 [P1] レスポンス形式の統一
  - error-response関数の修正
  - success-response関数の修正
  - set-responseの使用
- [x] T013 [P1] リクエストボディの処理修正
  - JSON配列のベクター→リスト変換
  - raw-bodyの正しい取得方法

---

## Phase 4: バックエンド - ルーティング設定

**目的**: タグマージAPIのルーティングを設定

- [x] T014 [P1] ルーティング定義
  - `/api/v1/tags/merge` → `<tags-merge-controller>`
  - `/api/v1/tags/merge-to-new` → `<tags-merge-to-new-controller>`
- [x] T015 [P1] application-loaderの更新
  - 新しいコントローラーをロード

---

## Phase 5: フロントエンド - Tag Merge API クライアント

**目的**: フロントエンド用のタグマージAPIクライアントを実装

- [x] T016 [P1] Merge型定義
  - TypeScript型定義
  - MergeToExistingRequest
  - MergeToNewRequest
  - MergeToExistingResponse
  - MergeToNewResponse
  - MergedTag
- [x] T017 [P1] tagsApiクライアントの拡張
  - `mergeToExisting(data)` - 既存タグへのマージ
  - `mergeToNew(data)` - 新規タグへのマージ
- [x] T018 [P1] APIクライアントのエラーハンドリング
  - バリデーションエラー
  - サーバーエラー

---

## Phase 6: フロントエンド - タグマージページ

**目的**: タグマージページを実装

- [x] T019 [P1] TagMergePageコンポーネント作成
  - ソースタグ選択UI
  - マージ先選択UI（既存 or 新規）
  - マージ実行ボタン
- [x] T020 [P1] ソースタグ選択機能
  - 複数タグ選択
  - 選択済みタグの表示
  - 選択解除機能
- [x] T021 [P1] マージ先選択機能
  - 「既存のタグにマージ」ラジオボタン
  - 既存タグのドロップダウン
  - 「新しいタグを作成してマージ」ラジオボタン
  - 新規タグ名入力フィールド
  - カラーピッカー
- [x] T022 [P1] バリデーション
  - 最低1つのソースタグ必須
  - マージ先の選択必須
  - 新規タグ作成時は名前必須
- [x] T023 [P1] マージ実行機能
  - 確認ダイアログ
  - API呼び出し
  - 成功時のリダイレクト
  - エラーハンドリング

---

## Phase 7: フロントエンド - UI/UX改善

**目的**: タグマージページのUI/UXを改善

- [x] T024 [P1] レスポンシブデザイン
  - モバイル対応
  - タブレット対応
- [x] T025 [P2] ダークモード対応
  - 既存デザインと統一
  - タグ色の視認性確保
  - モーダルのダークモード対応
- [x] T026 [P1] タグ選択UIの改善
  - div全体をクリック可能に（チェックボックスだけでなく）
  - 選択状態の視覚的フィードバック
- [x] T027 [P2] タグ一覧のダークモード対応
  - background/colorの統一
  - タグカードの見やすさ改善

---

## Phase 8: フロントエンド - ルーティング設定

**目的**: タグマージページのルーティングを設定

- [x] T028 [P1] ルート追加
  - `/tags/merge` → TagMergePage
- [x] T029 [P1] ナビゲーション追加
  - タグ一覧ページに「タグをマージ」ボタンを追加

---

## Phase 9: フロントエンド - スタイリング

**目的**: タグマージ機能のスタイルを実装

- [x] T030 [P1] tag-merge.cssの作成
  - タグマージページのスタイル
  - ソースタグ選択エリアのスタイル
  - マージ先選択エリアのスタイル
- [x] T031 [P2] ダークモード用CSS変数
  - カラースキームの定義
  - メディアクエリでの切り替え
- [x] T032 [P2] アクセシビリティ対応
  - focus状態のスタイル
  - キーボード操作対応

---

## Phase 10: 統合とテスト

**目的**: すべてのコンポーネントを統合しテスト

- [x] T033 [P1] エンドツーエンドテスト（手動）
  - タグマージフロー（既存タグへ）
  - タグマージフロー（新規タグへ）
  - バリデーションエラーの確認
- [x] T034 [P1] データ整合性の確認
  - TODOタグの移行確認
  - ラベルタグの移行確認
  - マージ済みタグの非表示確認
- [x] T035 [P1] エッジケースの確認
  - 同じTODOに既に対象タグが付いている場合
  - 同じラベルに既に対象タグが付いている場合
  - マージチェーンの解決

---

## Phase 11: ドキュメント

**目的**: ドキュメントを更新

- [ ] T036 [P2] specification.mdを作成
  - タグマージ機能の仕様書
- [ ] T037 [P2] API documentationを更新
  - タグマージAPIの説明
- [ ] T038 [P2] データベーススキーマドキュメントを更新
  - merged_atカラムの説明

---

## 進捗管理

### Phase 1: データベースとモデル
- 進捗: 3/3
- 状態: 完了 ✅

### Phase 2: バックエンド - Tag Merge Service
- 進捗: 4/4
- 状態: 完了 ✅

### Phase 3: バックエンド - Tag Merge API エンドポイント
- 進捗: 6/6
- 状態: 完了 ✅

### Phase 4: バックエンド - ルーティング設定
- 進捗: 2/2
- 状態: 完了 ✅

### Phase 5: フロントエンド - Tag Merge API クライアント
- 進捗: 3/3
- 状態: 完了 ✅

### Phase 6: フロントエンド - タグマージページ
- 進捗: 5/5
- 状態: 完了 ✅

### Phase 7: フロントエンド - UI/UX改善
- 進捗: 4/4
- 状態: 完了 ✅

### Phase 8: フロントエンド - ルーティング設定
- 進捗: 2/2
- 状態: 完了 ✅

### Phase 9: フロントエンド - スタイリング
- 進捗: 3/3
- 状態: 完了 ✅

### Phase 10: 統合とテスト
- 進捗: 3/3
- 状態: 完了 ✅

### Phase 11: ドキュメント
- 進捗: 0/3
- 状態: 未着手

---

## 全体進捗

- 合計タスク数: 38
- 完了タスク数: 35
- 進捗率: 92.1%

---

## 実装済み機能

### バックエンド
1. ✅ データベーススキーマ（merged_atカラム）
2. ✅ タグマージサービス（validate, merge, resolve）
3. ✅ TODO/ラベルタグの移行処理
4. ✅ マージ済みタグの除外処理
5. ✅ タグマージAPIエンドポイント（既存/新規）
6. ✅ エラーハンドリングとバリデーション
7. ✅ トランザクション処理

### フロントエンド
1. ✅ タグマージページUI
2. ✅ ソースタグ選択機能（複数選択）
3. ✅ マージ先選択機能（既存/新規）
4. ✅ カラーピッカー統合
5. ✅ バリデーション
6. ✅ エラーハンドリング
7. ✅ ダークモード対応
8. ✅ レスポンシブデザイン
9. ✅ ルーティング設定

---

## 残タスク

### ドキュメント
- [ ] T036: specification.mdの作成
- [ ] T037: API documentationの更新
- [ ] T038: データベーススキーマドキュメントの更新

---

## メモ

### 実装上の重要ポイント
- merged_atはDATETIME型を使用（フレームワークが自動的にタイムスタンプを管理）
- SELECT-INSERTではなくquery+make-recordを使用（フレームワークのベストプラクティス）
- マージ済みタグは`find-tags-by-user`で自動的に除外される
- JSON配列はベクターとしてパースされるため、リストに変換が必要
- エラーレスポンスは`set-response`を使用（他のコントローラーと統一）

### バグ修正履歴
1. ✅ `env`の取得方法を`(ref controller :env)`から`(env controller)`に修正
2. ✅ `read-body-as-string`に`(getf env :raw-body)`を渡すように修正
3. ✅ source_ulidsをベクターからリストに変換
4. ✅ `dbi:do-sql`の引数をリストから個別引数に修正（後に削除）
5. ✅ merged_at型をINTEGERからDATETIMEに変更
6. ✅ SELECT-INSERTからquery+make-recordに変更
7. ✅ ダークモード対応（タグ一覧、タグマージページ、モーダル）
8. ✅ タグ選択UIの改善（div全体をクリック可能に）
