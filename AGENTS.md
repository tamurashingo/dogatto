# dogatto AGENTS.md file

CommonLisp実装は SBCL を用いる。
SBCL は roswell 経由で起動する。

```bash
ros run
```

以下のファイルは clails コマンドを用いて生成する。
- migration
  - `clails generate:migration MIGRATION-NAME`
- model
  - `clails generate:model MODEL-NAME`
    - このコマンドだと migration も同時に生成する
  - `clails generate:model MODEL-NAME --no-migration`
    - このコマンドだと model のみ生成し、 migration は生成しない
- controller
  - `clails generate:controller CONTROLLER-NAME`
- view
  - `clails generate:view VIEW-NAME`


##  方針

### clails について

以下のドキュメントを参照してください

- command
  - https://github.com/tamurashingo/clails/blob/develop/document/command.md
- model / migration / sql など
  - https://github.com/tamurashingo/clails/blob/develop/document/model.md
- view
  - https://github.com/tamurashingo/clails/blob/develop/document/view.md
- controller
  - https://github.com/tamurashingo/clails/blob/develop/document/controller.md
- environment
  - https://github.com/tamurashingo/clails/blob/develop/document/environment.md
- test
  - https://github.com/tamurashingo/clails/blob/develop/document/testing.md
- task
  - https://github.com/tamurashingo/clails/blob/develop/document/task.md
- logging
  - https://github.com/tamurashingo/clails/blob/develop/document/logging.md

### 全体

- 内部のコードに defstruct を使用しない。 plist もしくは class を使うこと
  - defstruct 宣言時にメソッドが生えるのを回避したいため

- パッケージ定義に use は使用しない
  - use を使用するのは cl のみ
  - それ以外のパッケージ外にあるシンボルを参照する場合は import-from で記述する
  - これは Java の import で * を使用しないのと同じ理由である

  - テストでは cl, rove, テスト対象となるパッケージを use して良い

- パッケージ定義やパッケージの移動ではキーワードパラメータは使用しない
  - 以下のように `#` を付けて指定する

```common-lisp
(in-package #:cl-user)
(defpacakge #:clails/foo
  (:use #:cl)
  (:export #:bar))
```

- export するシンボルは必要最小限のシンボルをexportする


- パッケージは package-inferred-system で定義する
  - 最終的には src/application-loader.lisp で import すれば読み込まれるようにする

- ソースコードのコメント、標準出力、エラー出力に用いる言語は英語とする。

- defun, defmethod 等の関数を定義する際の docstring は以下の形式とする。
  - 最初の一文でどういう処理なのかを完結に書く。
  - 詳細な補足が必要な場合は1行空けて詳細情報を記入する。
  - 引数については以下の情報を記入する
    - 引数
      - 引数名
      - 期待する型
      - 説明
    - 戻り値
      - 型
      - 説明
    - 例外
      - 例外クラス
      - 発生する条件など
  - 引数が複数の型を許容する場合は、同じ変数名で定義を記述する
  - 多値を返す場合は戻り値を複数記述する

**サンプル**: 引数の型、戻り値がそれぞれ1種類の場合

```common-lisp
(defun div (x y)
  "与えられた被除数を除数で割った整数を返す。

   値は0以上の値となる。除数が被除数より大きい場合は0を返す。
   除数が0の場合はruntime-errorを投げる。

   @param x [number] 被除数
   @param y [numberr] 除数
   @return [integer] 割ったかず
   @condition runtime-error 除数に0を指定したとき
   "
  ....)
```

**サンプル**: 引数の型が複数の場合

```common-lisp
(defun make-keyword (name)
  "文字列またはシンボルを受けとり、キーワードとして返す

   @param name [symbol] キーワードに変換するシンボル
   @param name [string] キーワードに変換する文字列。小文字を渡した場合はすべて大文字にしてからキーワード化する。
   @return [keyword] キーワード
   @condition not-convert-error 変換対象外の内容が渡された場合
   "
   ...)

```

- クラス名は < と > で囲うこと

**サンプル**

```common-lisp
(defclass <foo> ()
  ...)
```


- カラム名は kebab-case とすること

**サンプル**

```common-lisp
(defmigration "20240101-120000-create-users-table"
  (:up #'(lambda (conn)
           (create-table conn :table "users"
                              :columns '(("name" :type :string
                                                 :not-null T)
                                         ("email" :type :string
                                                  :not-null T)
                                         ("age" :type :integer
                                                :not-null NIL)
                                         ("is-active" :type :boolean
                                                      :default-value T))))
   :down #'(lambda (conn)
             (drop-table conn :table "users"))))

(ref user :is-active)
```




