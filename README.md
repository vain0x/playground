# morphdom-examples

[morphdom](https://github.com/patrick-steele-idem/morphdom) というライブラリを使うことによって、サーバーサイドの実装に依存せずに宣言的 UI を実現できることのサンプルです。

## 仕組み

- 更新時に、サーバーにリクエストを送信して「更新後の HTML」を Ajax (`fetch`) で取得します。
- `morphdom` が「現在の DOM の状態」を「更新後の HTML」に合わせるように更新します。
    - 不足する要素の挿入や変化した属性の書き換えなど、最小限の更新だけ行われます。

## 利点

- 既存コードとの相互運用
    - テンプレートエンジンで HTML を生成する従来のウェブアプリケーションに容易に組み込めます。
- 疎結合 (React/Vue と比べて)
    - JSX のような特殊な構文を使用しないため、UI の構築が言語やライブラリに依存しません。
    - HTML の生成は文字列連結でもテンプレートエンジンでも何でもOK。
- 高速 (ページリロードと比べて)
    - ページ全体をリロードして更新するよりは、高速に動作するはずです。
- 簡潔 (jQuery と比べて)
    - jQuery や Web API により手続き的に DOM の状態更新を行うよりは、短いコードで機能を実現できるはずです。

## サンプル

examples ディレクトリを参照。

例えば「チェックボックスにチェックがついていないと無効になるボタン」は以下のように書けます。jQuery で disabled 属性を着脱するより宣言的です。

```js
    const render = accepted => `
        <label>
            <input type="checkbox name="accepted" ${accepted ? "checked" : ""}>
            Accepted
        </label>

        <button type="button" ${accepted ? "disabled": ""}>
            OK
        </button>
    `
```
