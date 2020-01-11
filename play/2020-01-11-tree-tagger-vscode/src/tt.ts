import { exec } from "child_process"

// FIXME: load from settings
const COMMAND = "/home/owner/bin/tree-tagger/cmd/tree-tagger-english"

const TIMEOUT = 10 * 1000

export interface TaggingResult {
  output: string
  stderr: string
}

const parseOutput = (tsv: string) => {
  return tsv
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
        output: parseOutput(stdout.toString('utf8')),
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
