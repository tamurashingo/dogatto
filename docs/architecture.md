# DOGATTO アーキテクチャ設計書

## 概要

DOGATTOは、Common Lisp (clails) バックエンドとReact フロントエンドを組み合わせた、タグベースのTODO管理Webアプリケーションです。

## アーキテクチャ方式

### フルSPA（Single Page Application）方式

DOGATTOは**フルSPA方式**を採用しています。これは、サーバーが初回アクセス時に単一のHTMLページとJavaScriptバンドルを返し、以降のページ遷移はすべてクライアントサイド（ブラウザ）で処理される方式です。

#### アーキテクチャの選択理由

1. **高速なページ遷移**: サーバーへのリクエストなしでページ遷移が可能
2. **モダンなUX**: スムーズなアニメーションとトランジション
3. **開発効率**: フロントエンドとバックエンドの関心の分離
4. **React Routerの活用**: クライアントサイドルーティングの標準ライブラリを使用

## システム構成

```
┌─────────────────────────────────────────────────────────────┐
│                         Browser                              │
│                                                              │
│  ┌────────────────────────────────────────────────────┐     │
│  │              React Application                      │     │
│  │                                                     │     │
│  │  ┌──────────────┐  ┌─────────────────────────┐    │     │
│  │  │ React Router │  │   Components            │    │     │
│  │  │ (Routing)    │  │ - LoginPage             │    │     │
│  │  │              │  │ - RegisterPage          │    │     │
│  │  │ /            │  │ - TodosPage             │    │     │
│  │  │ /login       │  │ - TagsPage              │    │     │
│  │  │ /register    │  │ - LabelsPage            │    │     │
│  │  │ /todos       │  │ ...                     │    │     │
│  │  │ /todos/:id   │  │                         │    │     │
│  │  └──────────────┘  └─────────────────────────┘    │     │
│  │                                                     │     │
│  │  ┌─────────────────────────────────────────┐      │     │
│  │  │         API Client                       │      │     │
│  │  │  - fetch wrapper                         │      │     │
│  │  │  - error handling                        │      │     │
│  │  │  - authentication                        │      │     │
│  │  └─────────────────────────────────────────┘      │     │
│  └────────────────────────────────────────────────────┘     │
│                          │                                   │
│                          │ HTTP/REST API                     │
└──────────────────────────┼───────────────────────────────────┘
                           │
┌──────────────────────────┼───────────────────────────────────┐
│                clails Server (Port 5000)                      │
│                                                              │
│  ┌────────────────────────────────────────────────────┐     │
│  │            Routing Layer                           │     │
│  │                                                     │     │
│  │  ┌──────────────────────────────────────────┐     │     │
│  │  │  HTML Serving (SPA Entry Point)          │     │     │
│  │  │  - GET /*  → pages/show/show.html        │     │     │
│  │  │              + /assets/javascript/index.js│     │     │
│  │  └──────────────────────────────────────────┘     │     │
│  │                                                     │     │
│  │  ┌──────────────────────────────────────────┐     │     │
│  │  │  REST API Endpoints                      │     │     │
│  │  │  - POST /api/v1/auth/register            │     │     │
│  │  │  - POST /api/v1/auth/login               │     │     │
│  │  │  - POST /api/v1/auth/logout              │     │     │
│  │  │  - GET  /api/v1/auth/me                  │     │     │
│  │  │  - CRUD /api/v1/todos/*                  │     │     │
│  │  │  - CRUD /api/v1/tags/*                   │     │     │
│  │  │  - CRUD /api/v1/labels/*                 │     │     │
│  │  │  ...                                      │     │     │
│  │  └──────────────────────────────────────────┘     │     │
│  └────────────────────────────────────────────────────┘     │
│                                                              │
│  ┌────────────────┐  ┌──────────────┐  ┌──────────────┐    │
│  │ Controllers    │  │ Models       │  │ Middleware   │    │
│  │ - auth         │  │ - user       │  │ - auth       │    │
│  │ - todos        │  │ - todo       │  │ - logging    │    │
│  │ - tags         │  │ - tag        │  │ - error      │    │
│  │ - labels       │  │ - label      │  │              │    │
│  └────────────────┘  └──────────────┘  └──────────────┘    │
└──────────────────────────────────────────────────────────────┘
                           │
        ┌──────────────────┴──────────────────┐
        │                                     │
┌───────▼────────┐                  ┌─────────▼────────┐
│  MySQL         │                  │  Redis           │
│  (Port 3306)   │                  │  (Port 6379)     │
│                │                  │                  │
│  - users       │                  │  - sessions      │
│  - todos       │                  │                  │
│  - tags        │                  │                  │
│  - labels      │                  │                  │
└────────────────┘                  └──────────────────┘
```

## リクエストフロー

### 1. 初回アクセス（例: ブラウザで `/todos` を開く）

```
1. ブラウザ
   → GET /todos
   → clailsサーバー

2. clailsサーバー (Routing Layer)
   - ワイルドカードルート `/*` にマッチ
   - pages-controller を実行
   
3. pages-controller
   → show.html を返す
   - <div id="root"></div>
   - <script src="/assets/javascript/index.js"></script>
   
4. ブラウザ
   ← HTML + JavaScript バンドル
   - JavaScriptを実行
   - Reactアプリケーションを起動
   
5. React Router
   - URLパス `/todos` を解析
   - router.tsx の定義に従い TodosPage コンポーネントをマウント
   
6. TodosPage コンポーネント
   - マウント時に useEffect でデータ取得
   → GET /api/v1/todos
   → clailsサーバー
   
7. clailsサーバー
   ← JSON データ
   
8. TodosPage コンポーネント
   - データをレンダリング
   - 画面表示完了
```

### 2. ページ遷移（例: `/todos` から `/login` へ移動）

```
1. ブラウザ（TodosPageを表示中）
   - ユーザーがリンクをクリック
   
2. React Router
   - <Link to="/login"> のクリックをインターセプト
   - サーバーへのリクエストを送らない
   - ブラウザのURLを `/login` に変更（History API使用）
   
3. React Router
   - router.tsx の定義に従い LoginPage コンポーネントをマウント
   
4. LoginPage コンポーネント
   - 即座にレンダリング（サーバーリクエストなし）
   - 画面表示完了
```

### 3. APIリクエスト（例: ログイン）

```
1. LoginPage コンポーネント
   - ユーザーがフォームを送信
   → POST /api/v1/auth/login
   → clailsサーバー
   
2. clailsサーバー (auth-controller)
   - 認証処理
   - セッションをRedisに保存
   - Cookie設定
   ← JSON レスポンス + Set-Cookie ヘッダー
   
3. LoginPage コンポーネント
   - ログイン成功
   - React Routerで `/todos` に遷移
   - （サーバーへのHTTPリクエストなし）
```

### 4. 直接URL入力/リロード（例: `/todos/:id` をリロード）

```
1. ブラウザ
   → GET /todos/01HXZ2M3N4P5Q6R7S8T9V0W1X2
   → clailsサーバー

2. clailsサーバー (Routing Layer)
   - ワイルドカードルート `/*` にマッチ
   - pages-controller を実行
   
3. pages-controller
   → show.html を返す（すべてのパスで同じHTML）
   
4. ブラウザ
   ← HTML + JavaScript バンドル
   - JavaScriptを実行
   - Reactアプリケーションを起動
   
5. React Router
   - URLパス `/todos/01HXZ2M3N4P5Q6R7S8T9V0W1X2` を解析
   - TodoDetailPage コンポーネントをマウント
   
6. TodoDetailPage コンポーネント
   → GET /api/v1/todos/01HXZ2M3N4P5Q6R7S8T9V0W1X2
   → clailsサーバー
   
7. clailsサーバー
   ← JSON データ
   
8. TodoDetailPage コンポーネント
   - データをレンダリング
   - 画面表示完了
```

## ルーティングの責任分担

### サーバーサイド（clails）の責任

1. **HTML配信**: すべてのパス（`/*`）で同じSPAエントリーポイントHTMLを返す
2. **静的ファイル配信**: JavaScriptバンドル、CSS、画像などを `/assets/*` で配信
3. **REST API**: `/api/*` でJSONデータの送受信
4. **認証・セッション管理**: Cookie/セッションの管理

### クライアントサイド（React Router）の責任

1. **URLとコンポーネントのマッピング**: どのパスでどのコンポーネントを表示するか
2. **ページ遷移の制御**: リンククリック時にサーバーリクエストを防ぐ
3. **ブラウザ履歴管理**: 戻る/進むボタンの対応
4. **動的ルーティング**: パラメータ付きURL（例: `/todos/:id`）の処理

## ファイル配置

### フロントエンド（開発時）

```
front/
├── src/
│   ├── main.tsx              # アプリケーションエントリーポイント
│   ├── router.tsx            # React Routerの設定
│   ├── App.tsx               # ルートコンポーネント
│   ├── pages/                # 各ページコンポーネント
│   │   ├── LoginPage.tsx
│   │   ├── RegisterPage.tsx
│   │   ├── TodosPage.tsx
│   │   └── ...
│   ├── components/           # 再利用可能なコンポーネント
│   ├── api/                  # APIクライアント
│   ├── contexts/             # React Context
│   ├── hooks/                # カスタムフック
│   └── types/                # TypeScript型定義
├── package.json
└── vite.config.ts            # Vite設定
```

### フロントエンド（ビルド後）

```
public/
└── assets/
    ├── javascript/
    │   └── index-[hash].js   # バンドルされたJavaScript
    ├── css/
    │   └── index-[hash].css  # バンドルされたCSS
    ├── images/               # 画像ファイル
    └── .vite/
        └── manifest.json     # ビルドマニフェスト
```

### バックエンド

```
app/
├── controllers/
│   ├── pages-controller.lisp      # HTML配信（SPA entry point）
│   ├── auth-controller.lisp       # 認証API
│   ├── todos-controller.lisp      # TODO API
│   └── ...
├── models/
│   ├── user.lisp
│   ├── todo.lisp
│   └── ...
├── views/
│   └── pages/
│       └── show/
│           └── show.html          # SPAのHTMLテンプレート
└── config/
    └── environment.lisp           # ルーティング設定
```

## 開発環境と本番環境の違い

### 開発環境

- **Vite開発サーバー** (port 3000): フロントエンド開発用
  - ホットリロード
  - 高速なビルド
  - `base: '/'` 設定
  - `historyApiFallback: true` でSPAルーティングをサポート
- **clailsサーバー** (port 5000): バックエンドAPI + HTML配信
  - HTML配信: Vite開発サーバーを参照する`<script>`タグを含むHTMLを返す
  - REST API提供
  - `VITE_DEV_SERVER_URL`環境変数で開発サーバーのURLを指定（デフォルト: `http://localhost:3000`）

**HTMLの動作:**
- `CLAILS_ENV=development` または `test`: Vite開発サーバーのアセットを参照
  ```html
  <script type="module" src="http://localhost:3000/@vite/client"></script>
  <script type="module" src="http://localhost:3000/src/main.tsx"></script>
  ```

### 本番環境

- **clailsサーバー** (port 5000): 
  - HTML配信（`/*` → `show.html`）
  - 静的ファイル配信（`/assets/*`）：ビルド済みアセット
  - REST API（`/api/*`）
  - `base: '/assets/'` 設定でビルド

**HTMLの動作:**
- `CLAILS_ENV=production`: ビルド済みアセット（ハッシュ付き）を参照
  ```html
  <link rel="stylesheet" href="/assets/css/index-[hash].css">
  <script type="module" src="/assets/javascript/index-[hash].js"></script>
  ```

## 設計上の利点

1. **パフォーマンス**: ページ遷移時のサーバーリクエストが不要
2. **ユーザー体験**: スムーズなページ遷移とアニメーション
3. **開発効率**: フロントエンドとバックエンドを独立して開発可能
4. **保守性**: 責任が明確に分離されている
5. **スケーラビリティ**: フロントエンドをCDNで配信することも可能

## 設計上の制約

1. **SEO**: クライアントサイドレンダリングのため、SEO対策が必要な場合は別途対応が必要
2. **初回ロード**: JavaScriptバンドルのサイズが大きいと初回ロードが遅い
3. **JavaScriptが必須**: JavaScriptが無効な環境では動作しない
4. **ブラウザ互換性**: モダンブラウザが必要

## 関連ドキュメント

- [API Conventions](./api-conventions.md) - REST API設計規約
- [Environment Variables](./environment.md) - 環境変数設定
- [Database Schema](./database.md) - データベース設計

---

**Version**: 1.0.0  
**Created**: 2026-01-14  
**Last Updated**: 2026-01-14
