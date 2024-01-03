import * as monaco from "monaco-editor/esm/vs/editor/editor.api"
import { useEffect, useRef } from "react"

// TODO: これだけだとwindow幅が変化してもeditorのサイズは変化しない

export const Editor = () => {
  const divRef = useRef<HTMLDivElement>(null)
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor>()

  useEffect(() => {
    const el = divRef.current
    if (el == null) return

    const editor = monaco.editor.create(el, {
      value: ["function x() {", `\tconsole.log("Hello world!")`, "}"].join("\n"),
      language: "typescript",
    })

    editorRef.current = editor
    return () => editor.dispose()
  }, [])

  return <div id="editor" ref={divRef} />
}
