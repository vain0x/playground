// サーバーサイド

// HTML 文字列の生成。
// 現実的にはテンプレートエンジン等を使うが、
// ここではシンプルに文字列連結で HTML を作る。

const h = s => String(s)
  .replace("<", "&lt;")
  .replace(">", "&gt;")
  .replace("\"", "&quot;")
  .replace("'", "&#39;")
  .replace("&", "&amp;")

const renderLayout = content => `
  <!DOCTYPE html>
  <html>

  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <title>Search</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="main.css"></style>
    <script src="main.js"></script>
  </head>

  <body>
    ${content}
  </body>

  </html>
`

const renderIndex = state => `
  <form id="app">
    <div>
      <input type="text" name="q" value="${h(state.q)}" placeholder="Filter">
    </div>

    <select name="selected">
      ${state.options.map(item => `
        <option
          data-key="${h(item.itemId)}"
          value="${h(item.itemId)}"
          ${state.selected === item.itemId ? "selected" : ""}>
          ${h(item.title)}
        </option>
      `).join("")}
    </select>

    <label>
      <input type="checkbox" name="accepted" ${state.accepted ? "checked" : ""}>
      Accept
    </label>

    <button type="button" ${state.accepted ? "" : "disabled"}>
      OK
    </button>
  </form>
`

const renderHtml = state => renderLayout(renderIndex(state))

module.exports = renderHtml
