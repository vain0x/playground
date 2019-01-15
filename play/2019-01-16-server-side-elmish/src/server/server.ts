import express from "express"
import * as path from "path"
import counter from "../pages/counter"

const { renderToString } = require("hyperapp-render")

const distDir = path.resolve(__dirname, "..", "..", "dist")

// FIXME: store model for each connection
let current: typeof counter

const dynamicRouter = () => {
  const r = express.Router()

  r.get("*", (_request, response) => {
    const node = counter.view(counter.model)
    const body = renderToString(node)

    const modelJson = JSON.stringify(counter.model)
    const nodeJson = JSON.stringify(node)

    current = counter

    const html = `<!DOCTYPE html>
      <html>
      <head>
          <meta charset="utf-8" />
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <title>My App</title>
          <meta name="viewport" content="width=device-width, initial-scale=1">
          <link rel="stylesheet" type="text/css" media="screen" href="/styles/index.css" />
      </head>
      <body>
          <div id="app-container">${body}</div>
          <script id="app-model" type="application/json">${modelJson}</script>
          <script id="app-node" type="application/json">${nodeJson}</script>
          <script src="/scripts/index.js"></script>
      </body>
      </html>
    `
    response.send(html)
  })

  r.post("/api/update", (request, response) => {
    try {
      console.debug("/api/update", request.body)

      const msg = request.body && request.body.msg
      const model = current.update(current.model, msg)
      const node = current.view(model)
      const next = { ...current, model }

      current = next
      return response.json({ node })
    } catch (err) {
      console.error("ERROR", err)
    }
  })

  return r
}

const fileRouter = () => {
  const fileHandler = express.static(distDir)

  const r = express.Router()

  r.get([
    "favicon.ico",
    "/styles/*",
    "/scripts/*",
    "/index.html",
  ], fileHandler)

  return r
}

export const serve = () => {
  const app = express()

  app.use(express.json())
  app.use(fileRouter())
  app.use(dynamicRouter())

  app.listen(8080, () => {
    console.debug("Listening to http://localhost:8080")
  })
}
