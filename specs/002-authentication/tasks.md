# 002-authentication タスクリスト

## 概要

このドキュメントは、ユーザー認証システム実装のためのタスクを定義します。

---

## Phase 1: バックエンド - パスワードハッシュ化とユーティリティ ✅

**目的**: パスワードを安全に処理するためのユーティリティを実装

- [x] T001 [P1] ironclad、babelをdogatto.asdに追加
- [x] T002 [P1] app/utils/password.lispを作成
- [x] T003 [P1] hash-password関数を実装（bcrypt/PBKDF2）
- [x] T004 [P1] verify-password関数を実装
- [x] T005 [P1] validate-password関数を実装（長さ、複雑さチェック）
- [x] T006 [P1] パスワードユーティリティのテストを作成

---

## Phase 2: バックエンド - セッション管理 ✅

**目的**: Redisを使用したセッションストレージを実装

- [x] T007 [P1] app/utils/session.lispを作成
- [x] T008 [P1] create-session関数を実装（Redisに保存）
- [x] T009 [P1] get-session関数を実装（Redisから取得）
- [x] T010 [P1] delete-session関数を実装
- [x] T011 [P1] session-valid?関数を実装（有効期限チェック）
- [x] T012 [P1] generate-session-id関数を実装（UUID）
- [x] T013 [P1] セッションユーティリティのテストを作成

---

## Phase 3: バックエンド - Userモデル ✅

**目的**: ユーザーデータを操作するためのモデル層を実装

- [x] T014 [P1] app/models/user.lispを作成
- [x] T015 [P1] find-user-by-email関数を実装
- [x] T016 [P1] find-user-by-id関数を実装
- [x] T017 [P1] create-user関数を実装
- [x] T018 [P1] update-user関数を実装
- [x] T019 [P1] delete-user関数を実装
- [x] T020 [P1] user-exists?関数を実装（メール重複チェック）
- [x] T021 [P1] Userモデルのバリデーションを実装
- [x] T022 [P1] Userモデルのテストを作成

---

## Phase 4: バックエンド - 認証コントローラー

**目的**: 認証エンドポイントを実装

- [ ] T023 [P1] clails generate:controller authでコントローラーを生成
- [ ] T024 [P1] POST /api/v1/auth/register エンドポイントを実装
  - 入力バリデーション
  - メール重複チェック
  - パスワードハッシュ化
  - ユーザー作成
  - レスポンス（パスワードハッシュを除外）
- [ ] T025 [P1] POST /api/v1/auth/login エンドポイントを実装
  - メールでユーザーを検索
  - パスワード検証
  - セッション作成
  - Cookieの設定
  - ユーザー情報を返す
- [ ] T026 [P1] POST /api/v1/auth/logout エンドポイントを実装
  - セッションID取得
  - セッション削除
  - Cookieクリア
- [ ] T027 [P1] GET /api/v1/auth/me エンドポイントを実装
  - セッションからユーザーID取得
  - ユーザー情報を返す
- [ ] T028 [P1] 認証コントローラーのテストを作成

---

## Phase 5: バックエンド - 認証ミドルウェア

**目的**: 保護されたルートのためのミドルウェアを実装

- [ ] T029 [P1] app/middleware/authentication.lispを作成
- [ ] T030 [P1] require-authentication関数を実装
  - Cookieからセッションid取得
  - セッション検証
  - envにユーザー情報を追加
  - 未認証の場合は401を返す
- [ ] T031 [P1] get-current-userヘルパー関数を実装
- [ ] T032 [P1] 認証ミドルウェアのテストを作成

---

## Phase 6: バックエンド - ルーティング設定

**目的**: 認証エンドポイントをルーティングに追加

- [ ] T033 [P1] app/config/environment.lispに認証ルートを追加
  - POST /api/v1/auth/register
  - POST /api/v1/auth/login
  - POST /api/v1/auth/logout
  - GET /api/v1/auth/me
- [ ] T034 [P1] application-loaderに認証関連ファイルを追加

---

## Phase 7: フロントエンド - ルーティング設定

**目的**: React Routerを設定

- [ ] T035 [P1] react-router-domをインストール
- [ ] T036 [P1] front/src/router.tsxを作成
- [ ] T037 [P1] ルート定義（/, /login, /register, /todos）
- [ ] T038 [P1] main.tsxでルーターを設定

---

## Phase 8: フロントエンド - 認証API

**目的**: 認証APIクライアントを実装

- [ ] T039 [P1] front/src/api/auth.tsを作成
- [ ] T040 [P1] register関数を実装
- [ ] T041 [P1] login関数を実装
- [ ] T042 [P1] logout関数を実装
- [ ] T043 [P1] getCurrentUser関数を実装
- [ ] T044 [P1] APIエラーハンドリングを実装

---

## Phase 9: フロントエンド - ログインページ

**目的**: ログインUIを実装

- [ ] T045 [P1] front/src/pages/LoginPage.tsxを作成
- [ ] T046 [P1] ログインフォームを実装
  - メール入力
  - パスワード入力
  - 送信ボタン
- [ ] T047 [P1] フォームバリデーションを実装
- [ ] T048 [P1] ログインAPIを呼び出し
- [ ] T049 [P1] エラーメッセージ表示を実装
- [ ] T050 [P1] ログイン成功時に/todosへリダイレクト
- [ ] T051 [P1] 登録ページへのリンクを追加

---

## Phase 10: フロントエンド - 登録ページ

**目的**: ユーザー登録UIを実装

- [ ] T052 [P1] front/src/pages/RegisterPage.tsxを作成
- [ ] T053 [P1] 登録フォームを実装
  - ユーザー名入力
  - メール入力
  - パスワード入力
  - パスワード確認入力
  - 送信ボタン
- [ ] T054 [P1] フォームバリデーションを実装
  - メールフォーマット
  - パスワードの強度
  - パスワード一致確認
- [ ] T055 [P1] 登録APIを呼び出し
- [ ] T056 [P1] エラーメッセージ表示を実装
- [ ] T057 [P1] 登録成功時に/loginへリダイレクト
- [ ] T058 [P1] ログインページへのリンクを追加

---

## Phase 11: フロントエンド - AuthContext更新

**目的**: 既存のAuthContextを実際の認証と統合

- [ ] T059 [P1] AuthContextに認証API統合
- [ ] T060 [P1] login関数を実装
- [ ] T061 [P1] logout関数を実装
- [ ] T062 [P1] refreshUser関数を実装（getCurrentUser呼び出し）
- [ ] T063 [P1] ローディング状態を追加
- [ ] T064 [P1] エラー状態を追加
- [ ] T065 [P1] アプリ起動時に現在のユーザーをロード

---

## Phase 12: フロントエンド - ProtectedRoute

**目的**: 認証が必要なルートを保護

- [ ] T066 [P1] front/src/components/ProtectedRoute.tsxを作成
- [ ] T067 [P1] 認証状態をチェック
- [ ] T068 [P1] 未認証の場合は/loginにリダイレクト
- [ ] T069 [P1] ローディング中の表示
- [ ] T070 [P1] ルーターでProtectedRouteを使用

---

## Phase 13: フロントエンド - ヘッダー/ナビゲーション

**目的**: ログアウトボタンを含むナビゲーションを実装

- [ ] T071 [P2] front/src/components/Header.tsxを作成
- [ ] T072 [P2] ユーザー名を表示
- [ ] T073 [P2] ログアウトボタンを実装
- [ ] T074 [P2] ログアウト成功時に/loginへリダイレクト

---

## Phase 14: 統合とテスト

**目的**: すべてのコンポーネントを統合しテスト

- [ ] T075 [P1] エンドツーエンドテスト
  - ユーザー登録フロー
  - ログインフロー
  - ログアウトフロー
  - 保護されたルートへのアクセス
- [ ] T076 [P1] エラーケースのテスト
  - 重複メール登録
  - 無効な認証情報でのログイン
  - セッション期限切れ
- [ ] T077 [P1] 統合の問題を修正

---

## Phase 15: UI/UXの改善

**目的**: ユーザーエクスペリエンスを向上

- [ ] T078 [P2] ログインページのスタイリング
- [ ] T079 [P2] 登録ページのスタイリング
- [ ] T080 [P2] ローディングインジケーターを追加
- [ ] T081 [P2] フォームバリデーションフィードバックを改善
- [ ] T082 [P2] エラーメッセージのスタイリング
- [ ] T083 [P2] レスポンシブデザイン

---

## Phase 16: ドキュメントとクリーンアップ

**目的**: ドキュメントを更新し、コードをクリーンアップ

- [ ] T084 [P2] README.mdに認証機能を追加
- [ ] T085 [P2] API documentationを更新
- [ ] T086 [P2] 未使用のコードを削除
- [ ] T087 [P2] コードコメントを追加
- [ ] T088 [P2] セキュリティレビュー

---

## タスク優先度

**P1（必須）**: コア機能、なければ動作しない  
**P2（重要）**: UX向上、後で実装可能

## 依存関係

```
Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5 → Phase 6
                                                    ↓
Phase 7 → Phase 8 → Phase 9, 10 → Phase 11 → Phase 12
                                                    ↓
                                              Phase 13
                                                    ↓
                                        Phase 14 → Phase 15 → Phase 16
```

## 推定工数

- Phase 1: 2時間
- Phase 2: 3時間
- Phase 3: 2時間
- Phase 4: 4時間
- Phase 5: 2時間
- Phase 6: 1時間
- Phase 7: 1時間
- Phase 8: 2時間
- Phase 9: 3時間
- Phase 10: 3時間
- Phase 11: 3時間
- Phase 12: 2時間
- Phase 13: 2時間
- Phase 14: 4時間
- Phase 15: 4時間
- Phase 16: 2時間

**合計**: 約40時間（1週間）
