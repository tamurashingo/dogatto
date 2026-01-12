# 002-authentication 実装計画

## プロジェクト概要

**機能**: ユーザー認証システム  
**開始日**: 2026-01-13  
**予定期間**: 7日間  
**優先度**: P1（必須）

---

## 1日目：バックエンド基盤

### 午前：パスワードハッシュ化（Phase 1）

**タスク**: T001〜T006

**作業内容**:
1. dogatto.asdに依存関係を追加
```lisp
:ironclad       ; 暗号化
:babel          ; 文字エンコーディング
```

2. `app/utils/password.lisp`を作成
   - `hash-password`: PBKDF2でパスワードをハッシュ化
   - `verify-password`: ハッシュとパスワードを照合
   - `validate-password`: 長さと複雑さをチェック

3. テストを作成
```lisp
;; test/utils/password.lisp
(deftest test-hash-password
  (let ((hash (hash-password "password123")))
    (ok (verify-password "password123" hash))
    (ng (verify-password "wrongpass" hash))))
```

### 午後：セッション管理（Phase 2）

**タスク**: T007〜T013

**作業内容**:
1. `app/utils/session.lisp`を作成
   - `create-session`: Redisにセッション保存、24時間TTL
   - `get-session`: セッションID でRedisから取得
   - `delete-session`: Redisからセッション削除
   - `session-valid?`: 有効期限チェック
   - `generate-session-id`: UUIDを生成

2. Redis接続を確認
```lisp
(defparameter *redis-connection* 
  (redis:connect :host "redis" :port 6379))
```

3. テストを作成

**成果物**:
- パスワードハッシュ化ユーティリティ
- セッション管理ユーティリティ
- 単体テスト

---

## 2日目：Userモデルと認証コントローラー

### 午前：Userモデル（Phase 3）

**タスク**: T014〜T019

**作業内容**:
1. `app/models/user.lisp`を作成
```lisp
(defclass <user> ()
  ((id :initarg :id :accessor user-id)
   (username :initarg :username :accessor username)
   (email :initarg :email :accessor email)
   (password-hash :initarg :password-hash :accessor password-hash)))

(defun find-user-by-email (email)
  "メールアドレスでユーザーを検索")

(defun find-user-by-id (id)
  "IDでユーザーを検索")

(defun create-user (username email password)
  "新しいユーザーを作成")

(defun user-exists? (email)
  "メールアドレスが既に使用されているか確認")
```

2. テストを作成

### 午後：認証コントローラー基礎（Phase 4開始）

**タスク**: T020〜T021

**作業内容**:
1. コントローラー生成
```bash
clails generate:controller auth
```

2. POST /api/v1/auth/register実装
```lisp
(defmethod do-post ((controller <auth-controller>))
  (let* ((params (parse-json-body))
         (username (getf params :username))
         (email (getf params :email))
         (password (getf params :password)))
    ;; バリデーション
    ;; メール重複チェック
    ;; ユーザー作成
    ;; レスポンス返却
    ))
```

**成果物**:
- Userモデル
- 登録エンドポイント

---

## 3日目：認証エンドポイント完成

### 午前：ログインエンドポイント（Phase 4続き）

**タスク**: T022

**作業内容**:
1. POST /api/v1/auth/login実装
```lisp
(defmethod do-post ((controller <auth-controller>))
  (let* ((params (parse-json-body))
         (email (getf params :email))
         (password (getf params :password))
         (user (find-user-by-email email)))
    (if (and user (verify-password password (password-hash user)))
        (let ((session-id (create-session (user-id user))))
          (set-cookie "session-id" session-id)
          (json-response user))
        (unauthorized-response))))
```

### 午後：ログアウト、meエンドポイント（Phase 4続き）

**タスク**: T023〜T025

**作業内容**:
1. POST /api/v1/auth/logout実装
2. GET /api/v1/auth/me実装
3. コントローラーテストを作成

**成果物**:
- 完全な認証API（4エンドポイント）
- 統合テスト

---

## 4日目：認証ミドルウェアとフロントエンド準備

### 午前：認証ミドルウェア（Phase 5, 6）

**タスク**: T026〜T031

**作業内容**:
1. `app/middleware/authentication.lisp`を作成
```lisp
(defun require-authentication (handler)
  (lambda (env)
    (let ((session-id (get-cookie env "session-id")))
      (if (session-valid? session-id)
          (let ((session (get-session session-id)))
            (funcall handler (plist-put env :current-user session)))
          (json-response '((:error . "Unauthorized")) :status 401)))))
```

2. ルーティング設定
3. application-loader更新

### 午後：フロントエンドルーティング（Phase 7, 8）

**タスク**: T032〜T041

**作業内容**:
1. react-router-domインストール
```bash
cd front
npm install react-router-dom@6
```

2. `front/src/router.tsx`作成
```typescript
import { createBrowserRouter } from 'react-router-dom';

const router = createBrowserRouter([
  { path: '/', element: <HomePage /> },
  { path: '/login', element: <LoginPage /> },
  { path: '/register', element: <RegisterPage /> },
  { path: '/todos', element: <ProtectedRoute><TodosPage /></ProtectedRoute> },
]);
```

3. `front/src/api/auth.ts`作成
```typescript
export async function login(email: string, password: string): Promise<User> {
  return apiClient.post('/auth/login', { email, password });
}

export async function register(username: string, email: string, password: string): Promise<User> {
  return apiClient.post('/auth/register', { username, email, password });
}

export async function logout(): Promise<void> {
  return apiClient.post('/auth/logout');
}

export async function getCurrentUser(): Promise<User> {
  return apiClient.get('/auth/me');
}
```

**成果物**:
- 認証ミドルウェア
- フロントエンドルーティング
- 認証APIクライアント

---

## 5日目：ログイン/登録ページ

### 午前：ログインページ（Phase 9）

**タスク**: T042〜T048

**作業内容**:
1. `front/src/pages/LoginPage.tsx`作成
```typescript
export function LoginPage() {
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [error, setError] = useState('');
  const { login } = useAuth();
  const navigate = useNavigate();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      await login(email, password);
      navigate('/todos');
    } catch (err) {
      setError('Invalid credentials');
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      {/* フォーム要素 */}
    </form>
  );
}
```

### 午後：登録ページ（Phase 10）

**タスク**: T049〜T055

**作業内容**:
1. `front/src/pages/RegisterPage.tsx`作成
2. バリデーション実装
3. パスワード確認チェック
4. エラーハンドリング

**成果物**:
- ログインページ
- 登録ページ

---

## 6日目：認証統合

### 午前：AuthContext統合（Phase 11）

**タスク**: T056〜T062

**作業内容**:
1. AuthContextを更新
```typescript
export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    // アプリ起動時に現在のユーザーをロード
    getCurrentUser()
      .then(setUser)
      .catch(() => setUser(null))
      .finally(() => setLoading(false));
  }, []);

  const login = async (email: string, password: string) => {
    const user = await authApi.login(email, password);
    setUser(user);
  };

  const logout = async () => {
    await authApi.logout();
    setUser(null);
  };

  return (
    <AuthContext.Provider value={{ user, login, logout, loading }}>
      {children}
    </AuthContext.Provider>
  );
}
```

### 午後：ProtectedRouteとヘッダー（Phase 12, 13）

**タスク**: T063〜T071

**作業内容**:
1. ProtectedRoute実装
```typescript
export function ProtectedRoute({ children }: { children: React.ReactNode }) {
  const { user, loading } = useAuth();

  if (loading) return <div>Loading...</div>;
  if (!user) return <Navigate to="/login" />;

  return <>{children}</>;
}
```

2. Headerコンポーネント作成

**成果物**:
- 完全に統合された認証システム
- ProtectedRoute
- ナビゲーション

---

## 7日目：テストと仕上げ

### 午前：統合テスト（Phase 14）

**タスク**: T072〜T074

**作業内容**:
1. エンドツーエンドテスト
   - 登録 → ログイン → TODO表示 → ログアウト
2. エラーケーステスト
3. セッション期限切れテスト

### 午後：UI改善とドキュメント（Phase 15, 16）

**タスク**: T075〜T085

**作業内容**:
1. スタイリング
2. ローディング状態
3. エラーメッセージ改善
4. ドキュメント更新
5. セキュリティレビュー

**成果物**:
- 完成した認証システム
- 更新されたドキュメント

---

## チェックリスト

### バックエンド
- [ ] パスワードハッシュ化が動作する
- [ ] セッションがRedisに保存される
- [ ] 登録エンドポイントが動作する
- [ ] ログインエンドポイントが動作する
- [ ] ログアウトエンドポイントが動作する
- [ ] meエンドポイントが動作する
- [ ] 認証ミドルウェアが動作する

### フロントエンド
- [ ] ログインページが動作する
- [ ] 登録ページが動作する
- [ ] ProtectedRouteが動作する
- [ ] ログアウトが動作する
- [ ] 認証状態が維持される
- [ ] エラーメッセージが表示される

### セキュリティ
- [ ] パスワードが平文保存されていない
- [ ] HttpOnly Cookieが設定されている
- [ ] セッションに有効期限がある
- [ ] SQLインジェクション対策済み
- [ ] XSS対策済み

---

## 成功メトリクス

1. **機能性**: すべてのエンドポイントが正常に動作
2. **セキュリティ**: パスワードハッシュ化、セッション管理が適切
3. **UX**: エラーメッセージが明確、スムーズなフロー
4. **テスト**: すべての主要フローがテスト済み
5. **ドキュメント**: API仕様が更新されている

---

## 次のステップ（Phase 1）

実装を開始しますか？最初のタスクは：

```bash
# Phase 1の開始
# T001: ironclad、babelをdogatto.asdに追加
```

準備ができたら実装を始めましょう！
