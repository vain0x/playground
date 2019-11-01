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

const termIsVar = (term: Term) => {
  return !term.name.match(/^[A-Z]/)
}

const termFresh = (): Term => ({
  name: "?" + freshId(),
  children: [],
})

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
  "button",
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

  srcElement.addEventListener("input", () => {
    const text = srcElement.value

    const [root, errors] = parse(text)
    rootElement.textContent = JSON.stringify(root, undefined, 2)
    if (errors.length !== 0) {
      errorsElement.textContent = errors.join("\n")
      return
    }

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

    const render = () => {
      const htmlTerm = termFresh()

      query({
        name: "view",
        children: [
          modelTerm,
          htmlTerm,
        ],
      })

      appElement.innerHTML = ""
      appElement.appendChild(termToNode(subst(htmlTerm)))
    }

    appElement.addEventListener("click", ev => {
      ev.preventDefault()

      const nextModelTerm = termFresh()

      query({
        name: "update",
        children: [
          modelTerm,
          {
            name: "Toggle",
            children: [],
          },
          nextModelTerm,
          termFresh(),
        ],
      })

      modelTerm = subst(nextModelTerm)
      render()
    })

    render()
  })
}
