import * as monaco from "monaco-editor"

const editorElement = document.getElementById("editor")!

const editor = monaco.editor.create(editorElement, {
  value: "Hello, world!",
  language: "semirich",
})
