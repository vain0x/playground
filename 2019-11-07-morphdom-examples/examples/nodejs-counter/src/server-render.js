// サーバーサイド

// HTML 文字列の生成。
// 現実的にはテンプレートエンジン等を使うが、
// ここではシンプルに文字列連結で HTML を作る。

const renderLayout = content => `
  <!DOCTYPE html>
  <html>

  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <title>Counter</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <script src="main.js"></script>
  </head>

  <body>
    ${content}
  </body>

  </html>
`

const renderIndex = state => `
  <form id="counter-form">
    <h2>${state.count}</h2>
    <button type="button" class="increment-button">+</button>
    <button type="button" class="decrement-button">-</button>
  </form>
`

const renderHtml = state => renderLayout(renderIndex(state))

module.exports = renderHtml
