import { startLsp } from "./lsp_connection"

console.error("Compiling wasm codes...")
const { lsp_request, lsp_notification, init_logger } = require("./pkg")

console.error("Starting LSP...")
startLsp(lsp_request, lsp_notification, init_logger)
