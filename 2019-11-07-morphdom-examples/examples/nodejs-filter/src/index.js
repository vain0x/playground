// クライアントサイド

import hydrate from "./client-hydrate"

document.addEventListener("DOMContentLoaded", () => {
  const dispatch = () => {
    const body = new FormData(document.querySelector("#app"))

    fetch("/", { method: "POST", body })
      .then(res => res.text())
      .then(html => hydrate(html, "#app"))
      .catch(err => console.error(err))
  }

  document.querySelector("#app")
    .addEventListener("input", ev => {
      if (ev.target.name === "q") {
        dispatch()
      }
    })

  document.querySelector("#app")
    .addEventListener("change", ev => {
      if (["accepted"].includes(ev.target.name)) {
        dispatch()
      }
    })
})
