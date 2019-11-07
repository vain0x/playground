document.addEventListener("DOMContentLoaded", () => {
  const formElement = document.querySelector("#counter-form")

  // action: どのボタンがクリックされたか
  // FIXME: リダイレクト対応、エラー処理など
  const dispatch = action => {
    const body = new FormData(formElement)
    body.append("action", action)

    fetch("/", { method: "POST", body })
      .then(res => res.text())
      .then(html => window.HydrateJs.hydrate(html))
      .catch(err => console.error(err))
  }

  document.querySelector(".increment-button")
    .addEventListener("click", () => dispatch("INCREMENT"))

  document.querySelector(".decrement-button")
    .addEventListener("click", () => dispatch("DECREMENT"))

  // `.text-input` に直接イベントをつけると増減したときにめんどうなので、
  // body までバブルしたものをフックする。(このあたりもライブラリ側で対応した方がいい。)
  document.body.addEventListener("change", ev => {
    if (ev.target.classList.contains("text-input")) {
      dispatch("VALIDATE")
    }
  })
})
