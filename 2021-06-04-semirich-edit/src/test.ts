import { getInitialState, lineTokenize } from "./semirich"

const test = (text: string) => {
  let state = getInitialState()

  for (const [row, line] of text.split(/\r?\n/g).entries()) {
    console.log("line:", row, line)
    const result = lineTokenize(line, state)
    state = result.endState
  }
}

test("{h: tag}\r\n{:100%}\\ %% {comment}\r\n{}}")
