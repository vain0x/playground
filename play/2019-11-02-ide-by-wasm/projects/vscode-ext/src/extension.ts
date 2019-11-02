/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

// Entry point of the VSCode extension.

import { ExtensionContext, workspace, commands } from "vscode"
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient"

let client: LanguageClient

const startLspClient = (context: ExtensionContext) => {
  const lspPath = context.asAbsolutePath("./out/lsp.js")
  let serverOptions: ServerOptions = {
    command: "node",
    args: [lspPath],
  }

  let clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "noupper-lang" },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
    },
  }

  // Start language server and client.
  client = new LanguageClient(
    "noupper-lang",
    "noupper-lang LSP",
    serverOptions,
    clientOptions
  )
  client.start()
}

export function activate(context: ExtensionContext) {
  startLspClient(context)
}

export function deactivate(): Thenable<void> | undefined {
  if (client) {
    return client.stop()
  }
}
