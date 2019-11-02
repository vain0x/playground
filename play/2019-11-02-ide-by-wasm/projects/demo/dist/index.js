const main = () => {
  fetch("example_lsp.wasm")
    .then(res => res.arrayBuffer())
    .then(bytes => WebAssembly.instantiate(bytes, {}))
    .then(results => results.instance.exports.main())
}

document.addEventListener("DOMContentLoaded", main)
