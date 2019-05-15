fetch("../out/main.wasm")
  .then(response => response.arrayBuffer())
  .then(bytes => WebAssembly.instantiate(bytes))
  .then(results => {
    const instance = results.instance
    const { f } = instance.exports
    const containerElement = document.getElementById("container")

    // Use exported function.
    containerElement.textContent = f()
  })
  .catch(console.error);
