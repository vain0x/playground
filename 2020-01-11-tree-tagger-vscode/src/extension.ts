import {
  languages,
  DocumentSelector,
  ExtensionContext,
  HoverProvider,
  TextDocument,
  Position,
  CancellationToken,
  ProviderResult,
  Hover,
  window,
} from "vscode"
import {
  positionInRange,
  tagging,
} from "./tt"

const doHover = async (document: TextDocument, position: Position, token: CancellationToken) => {
  await new Promise<void>(resolve => setTimeout(resolve, 300))

  if (token.isCancellationRequested) {
    return null
  }

  const text = document.getText()
  const result = await tagging(text)

  if (token.isCancellationRequested) {
    return null
  }

  const word = result.output.find(word => positionInRange(position, word.range))
  if (!word) {
    return null
  }

  const contents = [word.kind]

  if (!result.stderr) {
    contents.push(result.stderr)
  }

  return new Hover(contents, word.range)
}

class MyHoverProvider implements HoverProvider {
  public provideHover(document: TextDocument, position: Position, token: CancellationToken): ProviderResult<Hover | null> {
    return doHover(document, position, token).catch(err => {
      window.showWarningMessage(String(err))
      return null
    })
  }
}

export const activate = (context: ExtensionContext) => {
  const documentSelector: DocumentSelector = [
    {
      language: "plaintext",
    },
    {
      language: "markdown",
    },
  ]

  context.subscriptions.push(
    languages.registerHoverProvider(
      documentSelector,
      new MyHoverProvider()
    ))
}

export const deactivate = () => {
}
