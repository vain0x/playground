// サーバーサイド
// morphdom はサーバーの実装には依存しないので、どんな言語やフレームワークを使ってもいい。

const express = require("express")
const multer = require("multer")
const path = require("path")

const findItems = require("./server-data")
const renderHtml = require("./server-render")

const app = express()
const router = express.Router()

// 現在の状態。実際にはセッションか何かに持つ。
let state = {}

const generateIndexHtml = formData => {
  const options = findItems(formData.q)
  console.log({
    ...formData,
    ...state,
  })
  return renderHtml({
    ...formData,
    ...state,
    options,
  })
}

router.get("/", (_req, res) => {
  return res.send(generateIndexHtml({
    q: "",
    accepted: false,
  }))
})

router.post("/", (req, res) => {
  const formData = req.body
  state.selected = formData.selected && +formData.selected
  return res.send(generateIndexHtml(formData))
})

// allow 'multipart/form-data'
app.use(multer().none())

app.use(router)

const distDir = path.resolve(__dirname, "../dist")
app.use(express.static(distDir))

app.listen(8080, () => {
  console.log("http://localhost:8080")
})
