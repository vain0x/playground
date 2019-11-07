// カウンターアプリのサーバー
// hydrate-js はサーバーの実装には依存しないので、どんな言語やフレームワークを使ってもいい。

const path = require("path")
const express = require("express")
const multer = require("multer")

const app = express()
const router = express.Router()

// FIXME: エスケープ
const h = s => s

const validateText = str => /^[a-z]*$/.test(str)

const renderList = state => {
  const items = []
  for (let i = 0; i < state.count; i++) {
    // NOTE: express/multer の仕様で name=text[i] は配列になる
    items.push(`
      <li>
        ${i + 1}.
        <input
          type="text" class="text-input"
          name="text[${i}]"
          value="${h(state.text[i] || "")}">

        ${(validateText(state.text[i] || "")
          ? ""
          : `<span class="error">(only lowercase alphabets)</span>`)}
      </li>`)
  }
  return `<ul>${items.join("")}</ul>`
}

const renderHtml = state => (`
  <!DOCTYPE html>
  <html>

  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <title>Counter</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <script src="hydrate.js"></script>
    <script src="client.js"></script>
  </head>

  <body>
    <form id="counter-form" class="hydrate" action="/" method="POST">
      <input type="hidden" name="count" value="${state.count}">

      <h2>${state.count}</h2>
      <button type="button" class="increment-button">+</button>
      <button type="button" class="decrement-button">-</button>

      ${renderList(state)}
    </form>
  </body>

  </html>
`)

const INITIAL_STATE = {
  count: 0,
  text: [],
}

router.get("/", (_req, res) => {
  return res.send(renderHtml(INITIAL_STATE))
})

router.post("/", (req, res) => {
  console.debug(req.body)

  const state = req.body // validation is omitted for brevity
  if (state.action === "INCREMENT") {
    state.count++
  } else if (state.action === "DECREMENT") {
    state.count--
  } else if (state.action === "VALIDATE") {
    // pass
  } else {
    console.warn("unknown action", state.action)
  }

  state.text = state.text || []
  return res.send(renderHtml(state))
})

const hydrateJsPath = path.resolve(__dirname, "../../../src/index.js")
router.get("/hydrate.js", (_req, res) => {
  return res.sendFile(hydrateJsPath)
})

// allow 'multipart/form-data'
app.use(multer().none())

app.use(router)

const distDir = path.resolve(__dirname, "../dist")
app.use(express.static(distDir))

app.listen(8080, () => {
  console.log("http://localhost:8080")
})
