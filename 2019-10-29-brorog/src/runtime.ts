import { parse, Ast } from "./parse"

interface Term {
  name: string,
  children: Term[],
}

// -----------------------------------------------
// Term
// -----------------------------------------------

const freshId = (() => {
  let i = 0
  return () => i++
})()

const freshName = (name: string) => name + freshId()

const termIsVar = (term: Term) =>
  !term.name.match(/^[A-Z]/) && term.children.length === 0

const termFresh = (): Term => ({
  name: freshName("?"),
  children: [],
})

const termDoRefresh = (term: Term, rename: Map<string, string>): Term => {
  if (termIsVar(term)) {
    const name =
      rename.get(term.name)
      || rename.set(term.name, freshName(term.name)).get(term.name)!
    return { name, children: [] }
  }

  return {
    ...term,
    children: term.children.map(child => termDoRefresh(child, rename)),
  }
}

const termRefresh = (term: Term) =>
  termDoRefresh(term, new Map<string, string>())

// -----------------------------------------------
// Inference system
// -----------------------------------------------

const knowledge = new Map<string, Term[]>()

const env = new Map<string, Term>()

const bind = (varTerm: Term, valueTerm: Term) => {
  env.set(varTerm.name, valueTerm)
}

const subst = (term: Term): Term => {
  while (termIsVar(term)) {
    const bound = env.get(term.name)
    if (!bound) {
      break
    }

    term = bound
  }

  return {
    name: term.name,
    children: term.children.map(subst),
  }
}

const unify = (first: Term, second: Term): boolean => {
  first = subst(first)
  second = subst(second)

  if (termIsVar(first)) {
    if (termIsVar(second)) {
      if (first.name === second.name) {
        return true
      }
    }

    bind(first, second)
    return true
  }

  if (termIsVar(second)) {
    return unify(second, first)
  }

  if (first.name !== second.name
    || first.children.length !== second.children.length) {
    return false
  }
  for (let i = 0; i < first.children.length; i++) {
    if (!unify(first.children[i], second.children[i])) {
      return false
    }
  }
  return true
}

const query = (term: Term) => {
  const rules = knowledge.get(term.name) || []
  for (const rule of rules) {
    if (unify(term, termRefresh(rule))) {
      break
    }
  }
}

// -----------------------------------------------
// Evaluation
// -----------------------------------------------

const astToTerm = (ast: Ast): Term => {
  if (ast.kind !== "Term") {
    return termFresh()
  }

  return {
    name: ast.name,
    children: ast.children.map(astToTerm),
  }
}

const evalRoot = (root: Ast) => {
  if (root.kind !== "RootDecl") {
    return
  }

  for (const stmt of root.stmts) {
    if (stmt.kind !== "RuleStmt") {
      continue
    }

    if (stmt.term.kind !== "Term") {
      continue
    }

    const { name } = stmt.term

    const rules = knowledge.get(name) || knowledge.set(name, []).get(name)!
    rules.push(astToTerm(stmt.term))
  }
}

// -----------------------------------------------
// Browser runtime
// -----------------------------------------------

const HTML_TAGS = [
  "a",
  "article",
  "b",
  "button",
  "div",
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",
  "i",
  "input",
  "li",
  "section",
  "span",
  "ol",
  "textarea",
  "ul",
]

const termToNode = (term: Term): Node => {
  if (!HTML_TAGS.includes(term.name)) {
    return document.createTextNode(term.name)
  }

  const element = document.createElement(term.name)
  for (const child of term.children) {
    element.appendChild(termToNode(child))
  }
  return element
}

export const start = () => {
  const srcElement = document.getElementById("src") as HTMLTextAreaElement
  const rootElement = document.getElementById("root")!
  const errorsElement = document.getElementById("errors")!
  const appElement = document.getElementById("app")!

  const reload = () => {
    const text = srcElement.value

    const [root, errors] = parse(text)
    rootElement.textContent = JSON.stringify(root, undefined, 2)
    errorsElement.textContent = errors.join("\n")

    knowledge.clear()
    env.clear()

    evalRoot(root)

    let modelTerm = termFresh()

    query({
      name: "init",
      children: [
        modelTerm,
        termFresh(),
      ],
    })

    const update = (msgTerm: Term) => {
      const nextModelTerm = termFresh()

      query({
        name: "update",
        children: [
          modelTerm,
          msgTerm,
          nextModelTerm,
          termFresh(),
        ],
      })

      modelTerm = subst(nextModelTerm)
      render()
    }

    const render = () => {
      const htmlTerm = termFresh()

      query({
        name: "view",
        children: [
          modelTerm,
          htmlTerm,
        ],
      })

      const termElement = termToNode(subst(htmlTerm))
      termElement.addEventListener("click", ev => {
        ev.preventDefault()
        update({ name: "Toggle", children: [] })
      })

      appElement.innerHTML = ""
      appElement.appendChild(termElement)
    }

    render()
  }

  let id = 0
  srcElement.addEventListener("input", () => {
    const theId = ++id

    setTimeout(() => {
      if (id === theId) {
        reload()
      }
    }, 100)
  })

  srcElement.value = `rule init [
    Like
    _
]

rule update [
    Like
    Toggle
    Dislike
    _
]

rule update [
    Dislike
    Toggle
    Like
    _
]

rule view [
    msg
    button [
        msg
    ]
]`

  reload()
}
