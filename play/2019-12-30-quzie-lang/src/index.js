// TODO リストのサンプル

const quzie = require("./quzie")

const main = () => {
  const q = new quzie.QuzieRuntime()

  // let items = [];
  const itemsLocal = q.newLocal("items")
  q.setValue(itemsLocal, q.newArray())

  const addButtonElement = document.getElementById("add-button")
  const itemListElement = document.getElementById("item-list")
  let lastId = 0

  addButtonElement.addEventListener("click", () => {
    lastId++
    const id = lastId

    // items.push("");
    q.addItem(itemsLocal, q.newString(`Item ${id}`))
  })

  q.subscribe(itemsLocal, ([delta, index, value]) => {
    switch (delta) {
      case "D_ARRAY_INSERT": {
        const child = document.createElement("li")
        child.innerHTML = `<input type="text" value="${value}">`

        if (index === itemListElement.children.length) {
          itemListElement.appendChild(child)
        } else {
          itemListElement.insertBefore(child, itemListElement.children[index])
        }
        return
      }
      default:
        console.error(delta)
    }
  })
}

document.addEventListener("DOMContentLoaded", main)
