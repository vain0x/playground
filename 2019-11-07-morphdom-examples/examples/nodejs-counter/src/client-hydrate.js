// クライアントサイドのヘルパー

import morphdom from "morphdom"

/**
 * HTML 文字列から新しい Document オブジェクト (DOM) を生成する。
 */
const parseHtml = html => {
  const parser = new window.DOMParser()
  return parser.parseFromString(html, "text/html")
}

/**
 * DOM 更新処理。
 *
 * 現在の document と、与えられた HTML 文字列が表す DOM を比較して、
 * 前者に最小限の変更を施すことで後者と同じ状態にする。
 */
const hydrate = (html, query) => {
  const oldElement = document.querySelector(query)
  if (!oldElement) {
    console.error("Missing element to hydrate")
    return
  }

  const newDocument = parseHtml(html)
  const newElement = newDocument.querySelector(query)

  morphdom(oldElement, newElement)
}

export default hydrate
