// TODO リストのサンプル

const quzie = require("./quzie")

const main = () => {
  const q = new quzie.QuzieRuntime()

  // let locked = false;
  const lockedLocal = q.newLocal("locked", q.newBool(false))

  // let items = [];
  const itemsLocal = q.newLocal("items", q.newArray())

  const addButtonElement = document.getElementById("add-button")
  const itemListElement = document.getElementById("item-list")
  const lockCheckboxElement = document.getElementById("lock-checkbox")
  let lastId = 0

  addButtonElement.addEventListener("click", () => {
    lastId++
    const id = lastId

    // items.push("");
    q.addItem(itemsLocal, q.newString(`Item ${id}`))
  })

  lockCheckboxElement.addEventListener("change", ev => {
    q.setValue(lockedLocal, q.newBool(ev.target.checked))
  })

  q.subscribe(lockedLocal, value => {
    console.log(value, lockCheckboxElement.checked)
    if (lockCheckboxElement.checked !== value) {
      lockCheckboxElement.checked = value
    }
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
