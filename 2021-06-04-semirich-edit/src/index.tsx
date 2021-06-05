import * as monaco from "monaco-editor"
import { registerSemirichLanguage } from "./semirich"

const INITIAL_TEXT = `
%% 見出し
{h:
    やさい
}

%% 段落
{p:
    からだに{いい}。
}

%% セット
{set:
    {じゃがいも}
    {スイカ}
}



{hr:}

{h:
    くだもの
}

{p:
    あまくておいしい。\
    たべすぎ{ぷよぷよ}。
}

{list:
    {いちご}
    {バナナ}
}`

const editorElement = document.getElementById("editor")!

registerSemirichLanguage(monaco)

const e = monaco.editor.create(editorElement, {
  value: INITIAL_TEXT,
  language: "semirich",
})
