// クライアントサイド

import hydrate from "./client-hydrate"

document.addEventListener("DOMContentLoaded", () => {
  /**
   * 更新のリクエストをサーバーに送る。
   *
   * @param action 更新内容を表す文字列
   */
  const dispatch = action => {
    const body = new FormData()
    body.append("action", action)

    fetch("/", { method: "POST", body })
      .then(res => res.text())
      .then(html => {
        // 受信した HTML に基づいて DOM を更新する。
        // (`#counter-form` 要素だけ更新する。)
        hydrate(html, "#counter-form")
      })
      .catch(err => console.error(err))
  }

  // ボタンが押されたら押されたボタンの種類に基づいて更新する。
  document.querySelector(".increment-button")
    .addEventListener("click", () => dispatch("INCREMENT"))

  document.querySelector(".decrement-button")
    .addEventListener("click", () => dispatch("DECREMENT"))
})
