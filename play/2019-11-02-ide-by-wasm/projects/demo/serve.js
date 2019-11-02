const express = require("express")
const path = require("path")

const app = express()

const rootDir = path.resolve(__dirname, "../..")
const distFileServer = express.static(path.join(rootDir, "projects/demo/dist"))

const wasmPath = path.join(rootDir, "target/wasm32-unknown-unknown/debug/example_lsp.wasm")
console.log(wasmPath)

const router = express.Router()

router.get("/example_lsp.wasm", (_req, res) => {
  console.log("GET /example_lsp.wasm")
  return res.sendFile(wasmPath, {
    headers: {
      "Content-Type": "application/wasm",
    }
  })
})

app.use(router)
app.use(distFileServer)

app.listen(8080, () => {
  console.log("listening http://localhost:8080")
})
