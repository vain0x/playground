import { newWebRouter, newWebServer } from "./web_main"

const main = () => {
  const terminate = () => process.exit(0)
  process.once("SIGINT", terminate)
  process.once("SIGTERM", terminate)

  const router = newWebRouter()
  const server = newWebServer({ getRouter: () => router })

  server.start()
}

main()
