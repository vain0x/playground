# HydrateJs

実装状況: proof-of-concepts レベル

HydrateJS は DOM 自動更新ライブラリです。

## 仕組み

- 更新時に「現在の状態」をサーバーに送信して「更新後の HTML」を ajax (fetch) で取得します。
- `hydrate` 関数に更新後のHTMLを渡すと、「現在の DOM の状態」と「更新後の HTMLを比較して、異なる部分を更新します。
    - (内部的に insertBefore や setAttribute などの DOM 操作が必要最小限に行われます。)

## サンプル

examples ディレクトリを参照。
