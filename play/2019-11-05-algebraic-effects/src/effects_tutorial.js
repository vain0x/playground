// Based on [handlers-tutorial.pdf](https://www.eff-lang.org/handlers-tutorial.pdf)

const Var = name => ({ kind: "var", name })

const Str = value => ({ kind: "str", value })

const Unit = Str("")

const Fun = (arg, body) => ({ kind: "fun", arg, body })

const NativeFun = (name, body) => ({ kind: "native", name, body })

const Handler = (name, arg, kont, body) => ({ kind: "handler", name, arg, kont, body })

const Return = arg => ({ kind: "return", arg })

const Op = (name, arg, result, next) => ({ kind: "op", name, arg, result, next })

const Do = (name, body, next) => ({ kind: "do", name, body, next })

const App = (cal, arg) => ({ kind: "app", cal, arg })

const With = (handler, body) => ({ kind: "with", handler, body })

// Generic effect: op === fun x -> op(x; y. return y)
// const Geff = name => Fun("x", Op(name, Var("x"), "y", Return("y")))

// const Seq = (...args) => {
//   let next = args[args.length - 1]
//   for (let i = args.length - 1; i >= 1;) {
//     i--

//     let name = freshName()
//     next = Do(name, args[i], next)
//   }
//   return next
// }

const subst = (n, src, dest) => {
  if (src === "_") {
    return n
  }

  switch (n.kind) {
    case "var":
      return n.name === src ? dest : n

    case "fun":
      if (n.arg === src) {
        return n
      }
      return { ...n, body: subst(n.body, src, dest) }

    case "handler":
      if (n.arg === src || n.kont === src) {
        return n
      }
      return { ...n, body: subst(n.body, src, dest) }

    case "return":
      return { ...n, arg: subst(n.arg, src, dest) }

    case "op":
      n = { ...n, arg: subst(n.arg, src, dest) }
      if (n.result === src) {
        return n
      }
      return { ...n, next: subst(n.next, src, dest) }

    case "do":
      n = { ...n, body: subst(n.body, src, dest) }
      if (n.name === src) {
        return n
      }
      return { ...n, next: subst(n.next, src, dest) }

    case "app":
      return {
        ...n,
        cal: subst(n.cal, src, dest),
        arg: subst(n.arg, src, dest),
      }

    case "with":
      return {
        ...n,
        handler: subst(n.handler, src, dest),
        body: subst(n.body, src, dest),
      }

    case "str":
    case "native":
      return n

    default:
      throw new Error(`Unknown node: ${n.kind}`)
  }
}

// Reduce a node by 1 step.
// Return `null` if no rule applied.
const reduce = n => {
  switch (n.kind) {
    case "do": {
      const body = reduce(n.body)
      if (body) {
        return { ...n, body }
      }

      if (n.body.kind === "return") {
        return subst(n.next, n.name, n.body.arg)
      }

      if (n.body.kind === "op") {
        return {
          ...n.body,
          next: {
            ...n,
            body: n.body.next,
          },
        }
      }
      return null
    }

    case "app": {
      const cal = reduce(n.cal)
      if (cal) {
        return { ...n, cal }
      }

      const arg = reduce(n.arg)
      if (arg) {
        return { ...n, arg }
      }

      if (n.cal.kind === "fun") {
        return subst(n.cal.body, n.cal.arg, n.arg)
      }

      if (n.cal.kind === "native") {
        return n.cal.body(n.arg)
      }
      return null
    }

    case "with": {
      const body = reduce(n.body)
      if (body) {
        return { ...n, body }
      }

      if (n.body.kind === "return") {
        // FIXME: return handler is unimplemented
        return n.body
      }

      if (n.body.kind === "op" && n.body.name === n.handler.name) {
        const kont = Fun(n.body.result, { ...n, body: n.body.next })
        let m = subst(n.handler.body, n.handler.arg, n.body.arg)
        return subst(m, n.handler.kont, kont)
      }

      if (n.body.kind === "op") {
        return {
          ...n.body,
          next: {
            ...n,
            body: n.body.next,
          }
        }
      }
      return null
    }

    case "var":
    case "str":
    case "fun":
    case "native":
    case "handler":
    case "return":
    case "op":
      return null

    default:
      throw new Error(`Unknown node: ${n.kind}`)
  }
}

const reduceMany = ({ builtin, debug }) => {
  const go = n => {
    debug(n)

    const m = reduce(n)
    if (m) {
      return go(m)
    }

    if (n.kind === "op") {
      const body = builtin[n.name]
      if (body) {
        return body(n.arg, result => {
          go(subst(n.next, n.result, result))
        })
      }
    }

    if (n.kind !== "return") {
      console.error(n)
    }
  }
  return go
}

// -----------------------------------------------
// Tests
// -----------------------------------------------

const assert = require("assert")
const is = assert.deepStrictEqual

const only = null

const debug = n => {
  // console.debug(n)
}

let pass = 0

const test = (name, body) => {
  if (!only || name.includes(only)) {
    body()
    pass++
  }
}

const pushOnPrint = stdout => (arg, next) => {
  if (arg.kind === "str") {
    stdout.push(arg.value)
    return next(Unit)
  }
}

test("bultinOp", () => {
  const n =
    Op("print", Str("A"), "x",
      Op("print", Var("x"), "_",
        Return(Unit)
      ))

  const stdout = []
  reduceMany({
    builtin: {
      print: pushOnPrint(stdout),
    },
    debug,
  })(n)

  is(stdout, ["A", ""])
})

test("nativeFunc", () => {
  const bang = NativeFun("bang", arg =>
    Return(Str(arg.value + "!")))

  const n =
    Do("x", App(bang, Str("hello")),
      Op("print", Var("x"), "_",
        Return(Unit)
      ))

  const stdout = []
  reduceMany({
    builtin: {
      print: pushOnPrint(stdout),
    },
    debug,
  })(n)
  is(stdout, ["hello!"])
})

test("printFullName", () => {
  const join = NativeFun("join", first =>
    Return(NativeFun("join1", second =>
      Return(Str(`${first.value} ${second.value}`))
    )))

  const returnOnRead = value =>
    Handler(
      "read",
      "_", "k",
      App(Var("k"), Str(value)))

  const run = n => {
    const stdout = []
    reduceMany({
      builtin: {
        print: pushOnPrint(stdout),
      },
      debug,
    })(n)
    return stdout
  }

  const printFullName =
    Op("print", Str("What is your forename?"), "_",
      Op("read", Unit, "forename",
        Op("print", Str("What is your surname?"), "_",
          Op("read", Unit, "surname",
            Do("join1", App(join, Var("forename")),
              Do("fullname", App(Var("join1"), Var("surname")),
                Op("print", Var("fullname"), "_",
                  Return(Unit)
                )))))))

  is(run(With(returnOnRead("Bob"), printFullName)), [
    "What is your forename?",
    "What is your surname?",
    "Bob Bob",
  ])
})

test("reverse", () => {
  const run = n => {
    const stdout = []
    reduceMany({
      builtin: {
        print: pushOnPrint(stdout)
      },
      debug,
    })(n)
    return stdout
  }

  const abc =
    Op("print", Str("A"), "_",
      Op("print", Str("B"), "_",
        Op("print", Str("C"), "_",
          Return(Unit)
        )))

  is(run(abc), ["A", "B", "C"])

  const reverse = Handler(
    "print",
    "s",
    "k",
    Do("_", App(Var("k"), Unit),
      Op("print", Var("s"), "_",
        Return(Unit)
      )))

  run(With(reverse, abc), ["C", "B", "A"])
})

console.error("Success!", { pass })
