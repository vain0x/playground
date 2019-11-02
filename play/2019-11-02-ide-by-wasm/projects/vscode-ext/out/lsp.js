const fs = require("fs")
const path = require("path")
const util = require("util")

const main = () => {
  const rootDir = path.resolve(__dirname, "../../..")
  const lspPath = path.join(rootDir, "target/wasm32-unknown-unknown/release/example_lsp.wasm")

  const lspWasm = util.promisify(fs.readFile)(lspPath)

  lspWasm
    .then(buffer => new Uint8Array(buffer))
    .then(bytes => {
      console.error("Compiling...")
      return WebAssembly.compile(bytes)
    })
    .then(module => {
      console.error("Compiled.")
      return new WebAssembly.Instance(module)
    })
    .then(instance => {
      console.error("instance: ", instance)
      return instance.exports.main()
    })
    .catch(err => console.error("FATAL ERROR", err))
}

main()
