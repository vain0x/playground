// サーバーサイド
// morphdom はサーバーの実装には依存しないので、どんな言語やフレームワークを使ってもいい。

const express = require("express")
const multer = require("multer")
const path = require("path")

const renderHtml = require("./server-render")

const app = express()
const router = express.Router()

// 現在の状態。実際にはセッションか何かに持つ。
let state = {
  count: 0,
}

router.get("/", (_req, res) => {
  return res.send(renderHtml(state))
})

router.post("/", (req, res) => {
  const { action } = req.body

  if (action === "INCREMENT") {
    state.count++
  } else if (action === "DECREMENT") {
    state.count--
  } else {
    console.warn("unknown action", action)
  }

  return res.send(renderHtml(state))
})

// allow 'multipart/form-data'
app.use(multer().none())

app.use(router)

const distDir = path.resolve(__dirname, "../dist")
app.use(express.static(distDir))

app.listen(8080, () => {
  console.log("http://localhost:8080")
})
