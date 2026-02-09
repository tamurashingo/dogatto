# Phase 6 実装プラン: フロントエンド - ページ

## 現状分析

2026-02-09時点で、Phase 6のタスクはすでにほぼ実装済みです：

### 実装済みファイル
- ✅ `front/src/pages/LabelsPage.tsx` - ラベル一覧ページ（10,367 bytes）
- ✅ `front/src/components/LabelCard.tsx` - ラベルカード
- ✅ `front/src/components/LabelFormModal.tsx` - ラベル作成/編集モーダル
- ✅ `front/src/components/LabelDeleteConfirm.tsx` - 削除確認ダイアログ
- ✅ `front/src/components/LabelFilter.tsx` - ラベルフィルター

### 実装済み機能（LabelsPage.tsx）
- ✅ ラベル一覧取得
- ✅ 検索機能（ラベル名/タグ名）
- ✅ フィルター機能（全て/使用中/未使用）
- ✅ ソート機能（名前、タグ数、TODO数、更新日）
- ✅ ソート順変更（昇順/降順）
- ✅ 統計情報表示
- ✅ ローディング状態
- ✅ エラー表示
- ✅ 空状態の実装

## Phase 6 タスク完了チェック

### T019 [P1] ラベル一覧ページの実装 ✅
**ステータス**: 実装済み

実装内容：
- [x] ページファイル作成 (`front/src/pages/LabelsPage.tsx`)
- [x] ページコンポーネント実装
  - [x] ラベル一覧取得（labelsApi.getLabels）
  - [x] 検索機能（ラベル名/タグ名切り替え、searchMode, searchQuery）
  - [x] フィルター機能（all/used/unused）
  - [x] ソート機能（name, tag_count, todo_count, updated_at）
  - [x] ソート順切り替え（asc/desc）
  - [x] 統計情報表示（totalLabels, usedLabels, unusedLabels）
- [x] 空状態の実装（labels.length === 0 時のメッセージ）
- [x] ローディング状態（isLoading）
- [x] エラー表示（error state）

### T020 [P2] ラベル詳細ページの実装（将来）
**ステータス**: 未実装（P2 = 優先度低、Phase 6で実装予定）

現在の実装：
- LabelsPage からラベルをクリックすると `/todos?label=${label.ulid}` に遷移
- 専用の詳細ページは未実装

**判断**: P2タスクのため、現時点では保留とする

### T021 [P1] 検索機能の実装 ✅
**ステータス**: 実装済み

実装内容：
- [x] 検索モード切り替え（label_name/tag_name）
  - Line 28: `const [searchMode, setSearchMode] = useState<'label_name' | 'tag_name'>('label_name')`
  - UIで切り替え可能
- [x] 検索入力フォーム
  - Line 29: `const [searchQuery, setSearchQuery] = useState('')`
  - 検索フォーム実装済み
- [x] クリアボタン
  - `handleClearSearch` 関数実装済み

**要確認**: Debounce処理（300ms）の実装状況

### T022 [P1] フィルター・ソート機能の実装 ✅
**ステータス**: 実装済み

実装内容：
- [x] フィルタードロップダウン（all/used/unused）
  - Line 30: `const [filter, setFilter] = useState<'all' | 'used' | 'unused'>('all')`
- [x] ソートドロップダウン
  - Line 31: `const [sortBy, setSortBy] = useState<...>('name')`
  - 名前（name）
  - タグ数（tag_count）
  - TODO数（todo_count）
  - 更新日（updated_at）
- [x] ソート順変更（asc/desc）
  - Line 32: `const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('asc')`
  - `handleToggleSortOrder` 関数実装済み

**要確認**: URLクエリパラメータ連携の実装状況

## Phase 6 実施プラン

### ステップ1: 実装状況の詳細確認 ⏳
**目的**: tasks.md と実際の実装を照合し、未実装項目を洗い出す

**タスク**:
1. LabelsPage.tsx の全コードをレビュー
2. Debounce処理の実装確認
3. URLクエリパラメータ連携の確認
4. 空状態の実装詳細確認
5. エラーハンドリングの確認

**推定時間**: 30分

### ステップ2: 未実装機能の洗い出し ⏳
**目的**: T019-T022 で未実装の細かい機能を特定

**確認項目**:
- [ ] 検索のDebounce処理（300ms）
- [ ] 検索結果のハイライト
- [ ] URLクエリパラメータ連携（search, filter, sort等）
- [ ] ページネーション（tasks.mdに記載あり、実装状況不明）

**推定時間**: 15分

### ステップ3: 未実装機能の実装（必要に応じて） ⏳
**目的**: 発見された未実装機能を実装

**推定時間**: 1-3時間（未実装項目による）

### ステップ4: tasks.md の更新 ⏳
**目的**: Phase 6 の完了状況を正確に反映

**タスク**:
1. T019-T022 のチェックボックスを更新
2. 実績時間を記録
3. Phase 6 の完了ステータスを更新
4. 進捗サマリーを更新

**推定時間**: 15分

### ステップ5: テスト実施 ⏳
**目的**: Phase 6 の機能が正しく動作することを確認

**テスト項目**:
- [ ] ラベル一覧の表示
- [ ] 検索機能（ラベル名/タグ名）
- [ ] フィルター機能（全て/使用中/未使用）
- [ ] ソート機能（各項目、昇順/降順）
- [ ] 統計情報の表示
- [ ] ローディング状態
- [ ] エラー状態
- [ ] 空状態

**推定時間**: 30分

### ステップ6: Phase 6 完了報告 ⏳
**目的**: Phase 6 完了を正式に記録

**タスク**:
1. コミット作成
2. 完了報告ドキュメント作成（オプション）

**推定時間**: 15分

## 次のアクション

1. **即座に**: ステップ1（実装状況の詳細確認）を開始
2. LabelsPage.tsx の全体をレビュー
3. 未実装項目を特定

## 推定所要時間

- **最小**: 1.5時間（ほぼ完成している場合）
- **最大**: 4.5時間（複数の未実装項目がある場合）
- **想定**: 2時間（Debounce やクエリパラメータ等の小規模な実装が必要）

## リスク

### リスク1: ページネーション未実装の可能性
**影響度**: 中
**対応策**: 
- tasks.md には「ページネーション」が記載されているが、実装確認が必要
- 未実装の場合、簡易版（per_page固定）で対応も検討

### リスク2: URLクエリパラメータ未実装
**影響度**: 低
**対応策**:
- ブラウザの戻る/進むでフィルター状態が復元されない
- 実装する場合は react-router の useSearchParams を使用

## Phase 7 への準備

Phase 6 が完了次第、Phase 7（フロントエンド - コンポーネント）の状況も確認：
- LabelFormModal.tsx
- LabelCard.tsx
- LabelDeleteConfirm.tsx
- タグドロップダウンコンポーネント

これらも既に実装されている可能性が高いため、Phase 7 も迅速に完了できる見込み。

---

**作成日**: 2026-02-09  
**ステータス**: 📋 計画中  
**次のステップ**: ステップ1（実装状況の詳細確認）
