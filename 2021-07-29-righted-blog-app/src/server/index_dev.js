// サーバーを起動し、ソースコードが変更されるたびに自動で再起動する。
// 参考: <https://codeburst.io/dont-use-nodemon-there-are-better-ways-fc016b50b45e>

/* eslint-disable no-undef */
/* eslint-disable @typescript-eslint/no-var-requires */

const chokidar = require("chokidar")
const path = require("path")

const newWebRouter = () => require("../../target/server/web_main").newWebRouter()
const newWebServer = (...args) => require("../../target/server/web_main").newWebServer(...args)

const WATCHED_DIR = path.resolve(__dirname, "../../target")

// ===============================================

const clearCache = () => {
  for (const key of Object.keys(require.cache)) {
    if (!key.includes("node_modules")) {
      delete require.cache[key]
    }
  }
}

const main = async () => {
  const disposables = []
  const defer = d => disposables.push(d)

  const terminate = () => {
    disposables.forEach(d => d())
    process.exit(0)
  }
  process.once("SIGINT", terminate)
  process.once("SIGTERM", terminate)

  const start = () => {
    const watcher = chokidar.watch(WATCHED_DIR)
    defer(() => watcher.close())

    let router = null
    const server = newWebServer({
      getRouter: () => router ?? (console.log("index_dev: reloaded"), router = newWebRouter()),
    })
    const dispose = server.start()
    defer(dispose)

    watcher.once("ready", () => {
      watcher.on("all", () => {
        router = null
        clearCache()
      })
    })
  }

  while (true) {
    try {
      return start()
    } catch (err) {
      console.error("error:", err)
      disposables.splice(0, disposables.length).forEach(d => d())
      clearCache()
      await new Promise(resolve => setTimeout(resolve, 5000))
    }
  }
}

main()
