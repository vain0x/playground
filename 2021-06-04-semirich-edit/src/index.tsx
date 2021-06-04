import * as monaco from "monaco-editor"

const editorElement = document.getElementById("editor")!

monaco.editor.create(editorElement, {
  value: "Hello, world!",
})
