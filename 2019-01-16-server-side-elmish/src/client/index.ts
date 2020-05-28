import { h, app, VNode } from "hyperapp"

const appContainerElement = document.getElementById("app-container")!
const appModelElement = document.getElementById("app-model")!
const appNodeElement = document.getElementById("app-node")!

let globalActions: any

const wireEvents = (node: string | VNode): string | VNode => {
  if (typeof node !== "object" || node === null) return node

  const attributes: any = node.attributes || {}
  const change: any = {}

  for (const key in attributes) {
    if (attributes.hasOwnProperty(key)
      && key.startsWith("data-on")
      && typeof attributes[key] === "object"
      && attributes[key] !== null
      && !(attributes[key] instanceof Function)
    ) {
      const msg = attributes[key]
      const eventKey = key.slice("data-".length)

      console.log("[wireEvents] key=" + key)

      change[eventKey] = () => {
        globalActions.dispatch(msg)
      }
    }
  }

  const children = node.children.map(wireEvents)

  return { ...node, attributes: { ...attributes, ...change }, children }
}

const startHyperapp = () => {
  // const appModel = appModelElement !== null
  //   && appModelElement.textContent !== null
  //   ? JSON.parse(appModelElement.textContent)
  //   : {}

  let appNode = appNodeElement !== null
    && appNodeElement.textContent !== null
    ? JSON.parse(appNodeElement.textContent)
    : h("div")

  appNode = wireEvents(appNode)

  const actions = {
    set: (node: any) => node,
    dispatch: (msg: any) => (_state: any, actions: any) => {
      dispatchMsg(msg, actions.set)
    },
  }

  app(appNode, actions, (node, actions) => {
    console.debug("render", node, actions)
    globalActions = actions
    return node
  }, appContainerElement)
}

const dispatchMsg = (msg: any, next: any) => {
  console.debug("dispatch", msg)

  fetch("/api/update", {
    method: "POST",
    body: JSON.stringify({ msg }),
    credentials: "same-origin",
    headers: {
      "Content-Type": "application/json",
    },
  }).then(response => {
    if (!response.ok) {
      console.error("could not update", response.statusText)
      return
    }

    return response.json()
  }).then(({ node }) => {
    next(wireEvents(node))
  }).catch(err => {
    console.error("fatal error", err)
  })
}

const main = () => {
  startHyperapp()
}

main()
