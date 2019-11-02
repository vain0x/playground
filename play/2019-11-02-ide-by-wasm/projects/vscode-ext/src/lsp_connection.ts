import {
  createConnection,
  Logger,
} from "vscode-languageserver"

type LspRequestFun = (method: string, params: unknown) => unknown

type LspNotificationFun = (method: string, params: unknown) => unknown

type InitLoggerFun = () => void

export const startLsp = (lsp_request: LspRequestFun, lsp_notification: LspNotificationFun, init_logger: InitLoggerFun) => {
  const connection = createConnection();

  (connection as any)["publish_diagnostics"] = (params: any) => {
    connection.sendDiagnostics(params)
  };

  (global as any)["lsp_connection"] = connection
  init_logger()

  const onRequest = (method: string) => (params: unknown) => {
    console.error(JSON.stringify({ method, params }))
    return lsp_request(method, params) as any
  }

  const onNotifiaction = (method: string) => (params: unknown) => {
    lsp_notification(method, params)
  }

  connection.onInitialize(onRequest("initialize"))

  connection.onInitialized(onRequest("initialized"))

  connection.onDidOpenTextDocument(onNotifiaction("textDocument/didOpen"))

  connection.onDidChangeTextDocument(onNotifiaction("textDocument/didChange"))

  connection.onDidCloseTextDocument(onNotifiaction("textDocument/didClose"))

  console.error("Listening...")
  connection.listen()
}
