import { monaco } from "react-monaco-editor"

class MyState implements monaco.languages.IState {
  constructor(public readonly depth = 0) { }

  clone(): MyState {
    return new MyState(this.depth)
  }

  equals(other: monaco.languages.IState): boolean {
    if (!(other instanceof MyState)) return false
    return true
  }
}

const THE_STATE: monaco.languages.IState = new MyState()

const getInitialState = (): monaco.languages.IState => THE_STATE

const tokenize = (line: string, state: monaco.languages.IState): monaco.languages.ILineTokens => {
  if (!(state instanceof MyState)) {
    console.log("wrong state", state)
    return { tokens: [], endState: state }
  }

  const tokens: monaco.languages.IToken[] = []
  let i = 0
  while (i < line.length) {
    if (line[i] === " " || line[i] === "\r" || line[i] === "\t") continue


  }
  for (const word of line.split(" ")) {
    tokens.push({
      startIndex: i,
      scopes: word[0] === "\"" ? "string" : "none",
    })
    i += word.length
  }

  return {
    tokens,
    endState: THE_STATE,
  }
}

export const registerSemirichLanguage = () => {
  monaco.languages.setTokensProvider("semirich", { getInitialState, tokenize })

  monaco.languages.register({
    id: "semirich",
    aliases: [
      "セミリッチエディット",
    ],
    extensions: [
      ".semirich",
    ],
  })
}
