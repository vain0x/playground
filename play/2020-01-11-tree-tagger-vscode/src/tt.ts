import { exec } from "child_process"
import { Range, Position } from "vscode"

// FIXME: load from settings
const COMMAND = "/home/owner/bin/tree-tagger/cmd/tree-tagger-english"

const TIMEOUT = 10 * 1000

interface Word {
  kind: string
  range: Range
}

export interface TaggingResult {
  output: Word[]
  stderr: string
}

const positionCompare = (first: Position, second: Position) => {
  if (first.line !== second.line) {
    return Math.sign(first.line - second.line)
  }

  return Math.sign(first.character - second.character)
}

export const positionInRange = (position: Position, range: Range) =>
  positionCompare(range.start, position) <= 0
  && positionCompare(position, range.end) < 0

const parseOutput = (text: string, tsv: string) => {
  const lines = tsv.split(/\r\n|\n/)

  let words: Word[] = []

  const positions: Position[] = []
  {
    let line = 0
    let character = 0

    for (let i = 0; i < text.length; i++) {
      positions.push(new Position(line, character))

      if (text[i] === "\n") {
        line++
        character = 0
      } else {
        character++
      }
    }

    positions.push(new Position(line, character))
  }

  let index = 0

  for (const line of lines) {
    const [word, kind] = line.split("\t")

    if (!word || !kind) {
      continue
    }

    const shift = text.slice(index).indexOf(word)
    if (shift < 0) {
      continue
    }

    index += shift

    words.push({
      kind,
      range: new Range(positions[index], positions[index + word.length]),
    })

    index += word.length
  }

  return words
}

export const tagging = async (text: string) => {
  return new Promise<TaggingResult>((resolve, reject) => {
    let isResolved = false

    const process = exec(COMMAND, {
      encoding: 'buffer',
    }, (err, stdout, stderr) => {
      if (err) {
        reject(err)
        return
      }

      if (isResolved) {
        return
      }

      isResolved = true
      resolve({
        output: parseOutput(text, stdout.toString('utf8')),
        stderr: stderr.toString('utf8'),
      })
    })

    const stdin = process.stdin
    if (!stdin) {
      reject("STDIN is null")
      return
    }

    const input = Buffer.from(text)
    stdin.write(input, err => {
      if (err) {
        reject(err)
        return
      }

      stdin.end(() => {
        // it should exit in some time after this or timeout
        setTimeout(() => {
          if (!isResolved) {
            isResolved = true
            reject("tree-tagger timeout")
          }
        }, TIMEOUT)
      })
    })
  })
}
