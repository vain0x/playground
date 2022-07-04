const TRACE = false

// -----------------------------------------------
// ツリーの構造
// -----------------------------------------------

/**
 * ホスト要素の種類。
 *
 * `<div ... />` のようにJSXのタグとして書ける。
 */
type HostElementType = keyof JSX.IntrinsicElements

export const Fragment = "fragment"

type FnComponentType = (props: Props) => Element

/** 要素の種類 */
type ElementType = HostElementType | FnComponentType

/** 要素のプロパティ */
interface Props {
  children?: Element[]
  [key: string]: unknown
}

/**
 * 要素。
 *
 * `<x>...</x>` の型。
 */
export interface Element {
  type: ElementType
  props: Props & { children?: Element[] }
}

/**
 * 要素の子ノード。
 *
 * `<x>...</x>` の中に書けるもの。
 */
export type Node = string | number | boolean | Element | null | undefined

// Element.children をすべてElementにするため、エレメントでない子ノードをすべてtext要素で包む。
const wrapNode = (node: Node): Element[] => {
  if (Array.isArray(node)) {
    return flatMap(node, wrapNode)
  }

  if (typeof node === "object" && node != null && node.type != null) {
    return [node]
  }

  return [{ type: "text", props: { value: node } }]
}

/**
 * 要素を作る。
 *
 * JSX記法で `<x>...</x>` という式は `createElement` の呼び出しに変形される。
 */
export const createElement = (type: ElementType, props: Props, ...children: Node[]): Element => ({
  type,
  props: {
    ...props,
    children: flatMap(children, wrapNode),
  },
})

// -----------------------------------------------
// hooks
// -----------------------------------------------

let workingRoot: RootCtx | undefined
let workingFiber: Fiber | undefined

type Dispatch<T> = (value: T) => void

type UnknownHook =
  UseEffectHook
  | UseStateHook<unknown>

type DisposeFn = () => void
type EffectFn = () => DisposeFn | void
type Deps = readonly unknown[]

const depsEqual = (ls: Deps, rs: Deps): boolean =>
  ls.length === rs.length && ls.every((l, i) => Object.is(l, rs[i]))

interface UseEffectHook {
  kind: "useEffect"
  deps: Deps
  action?: EffectFn
  dispose?: DisposeFn
}

export const useEffect = (action: EffectFn, deps: Deps): void => {
  const wipFiber = workingFiber
  const ctx = workingRoot
  if (wipFiber == null || ctx == null) throw new Error("illegal use of useEffect")

  const hookIndex = wipFiber.hookIndex
  wipFiber.hookIndex++

  const oldHook = wipFiber.alternate?.hooks[hookIndex] as UseEffectHook | undefined
  let newHook: UseEffectHook
  if (oldHook != null) {
    newHook = oldHook

    TRACE && console.log("trace: effect equal?", oldHook.deps, deps, depsEqual(oldHook.deps, deps))
    newHook.action = !depsEqual(oldHook.deps, deps) ? action : undefined
    newHook.deps = deps
  } else {
    newHook = {
      kind: "useEffect",
      deps,
      action,
    }
  }

  wipFiber.hooks[hookIndex] = newHook
}

const updateEffects = (fiber: Fiber) => {
  for (const hook of fiber.hooks) {
    if (hook.kind === "useEffect" && hook.action != null) {
      if (hook.dispose != null) {
        const d = hook.dispose
        hook.dispose = undefined

        try {
          d()
        } catch (err) {
          console.error("error during effect cleanup", err)
        }
      }

      const action = hook.action
      hook.action = undefined

      let newDisposeFn: DisposeFn | void
      try {
        newDisposeFn = action()
      } catch (err) {
        console.error("error during effect action", err)
      }
      hook.dispose = newDisposeFn!
    }
  }
}

const disposeEffects = (fiber: Fiber) => {
  for (const hook of fiber.hooks) {
    if (hook.kind === "useEffect" && hook.dispose != null) {
      const d = hook.dispose
      hook.dispose = undefined

      try {
        d()
      } catch (err) {
        console.error("error during effect cleanup", err)
      }
    }
  }
}

interface UseStateHook<T> {
  kind: "useState"
  state: T
  setState: Dispatch<T>
  queue: T[]
}

export const useState = <T>(init: T): [T, Dispatch<T>] => {
  const wipFiber = workingFiber
  const ctx = workingRoot
  if (wipFiber == null || ctx == null) throw new Error("illegal use of useState")

  const hookIndex = wipFiber.hookIndex
  wipFiber.hookIndex++

  const oldHook = wipFiber.alternate?.hooks[hookIndex] as UseStateHook<T> | undefined
  let newHook: UseStateHook<T>
  if (oldHook != null) {
    newHook = oldHook

    if (newHook.queue.length !== 0) {
      newHook.state = newHook.queue[newHook.queue.length - 1]
      newHook.queue.length = 0
    }
  } else {
    newHook = {
      kind: "useState",
      state: init,
      setState: ((value: T): void => {
        newHook.queue.push(value)
        rerender(ctx)
      }),
      queue: [],
    }
  }

  wipFiber.hooks[hookIndex] = newHook as UnknownHook

  return [newHook.state, newHook.setState]
}

// -----------------------------------------------
// 内部状態
// -----------------------------------------------

export interface RootHost {
  /** 処理を中断すべきならtrue */
  interrupted: boolean

  /** 残りの処理を登録する。 */
  requestIdleCallback(callback: () => void): void
}

interface RootCtx extends RootHost {
  /** render途中のファイバー */
  nextUnitOfWork?: Fiber

  /** render完了後、commit完了前のファイバー*/
  wipRoot?: Fiber

  /** 最後にコミットしたファイバーツリー */
  currentRoot?: Fiber

  /** いまのレンダリングにおいて削除とマークされたファイバー */
  deletions: Fiber[]
}

interface Fiber {
  phase: "before" | "after"
  type: ElementType
  props: Props

  parent?: Fiber
  child?: Fiber
  sibling?: Fiber

  // 以前のファイバー
  alternate?: Fiber
  effectTag?: EffectKind

  // hook
  hooks: UnknownHook[]
  hookIndex: number
}

type EffectKind = "UPDATE" | "PLACEMENT" | "DELETION"

const elementToName = (element: { type: ElementType }): string =>
  typeof element.type === "string"
    ? element.type
    : typeof element.type === "function"
      ? element.type.name ?? element.type.toString()
      : `${element.type}`

// -----------------------------------------------
// reconciliation
// -----------------------------------------------

// 古い要素と新しい要素を同一の存在とみなして関連づけする操作

const reconcileChildren = (wipFiber: Fiber, elements: readonly Element[], ctx: RootCtx): void => {
  let oldFiber = wipFiber.alternate?.child

  let i = 0
  let prevSibling: Fiber | undefined

  while (i < elements.length || oldFiber != null) {
    const element: Element | undefined = elements[i]
    let newFiber: Fiber | undefined

    const sameType = oldFiber != null && element != null && element.type === oldFiber.type
    TRACE && console.log("reconcile", wipFiber.type, i, element?.type, oldFiber?.type, "same?", sameType)
    if (sameType) {
      newFiber = {
        type: oldFiber!.type,
        props: element.props,

        phase: oldFiber!.phase,
        parent: wipFiber,

        alternate: oldFiber,
        effectTag: "UPDATE",

        hooks: oldFiber!.hooks,
        hookIndex: 0,
      }
    } else {
      if (element != null) {
        newFiber = {
          type: element.type,
          props: element.props,

          phase: "before",
          parent: wipFiber,

          effectTag: "PLACEMENT",

          hooks: [],
          hookIndex: 0,
        }
      }
      if (oldFiber != null) {
        oldFiber.effectTag = "DELETION"
        ctx.deletions.push(oldFiber)
      }
    }

    oldFiber = oldFiber?.sibling

    if (i === 0) {
      wipFiber.child = newFiber
    } else if (element != null) {
      prevSibling!.sibling = newFiber
    }
    prevSibling = newFiber
    i++
  }
}

// -----------------------------------------------
// render
// -----------------------------------------------

const updateHostComponent = (fiber: Fiber & { type: string }, ctx: RootCtx): void => {
  // たぶんこのへんがこのfiber自身が行う要素への処理
  // (このタイミングで副作用を引き起こしてはならない。)
  // fiber.dom = createDom(fiber) // 再帰の前
  // if (fiber.parent != null) { // 再帰の後
  //   fiber.parent.dom.appendChild(fiber.dom)
  // }
  if (fiber.phase === "before") {
    TRACE && console.log("BEGIN", elementToName(fiber))
    fiber.phase = "after"
  } else {
    TRACE && console.log("END", elementToName(fiber))
  }

  const { children, ...props } = fiber.props as Props

  if (children != null) {
    reconcileChildren(fiber, children, ctx)
  }
}

const updateFnComponent = (fiber: Fiber & { type: FnComponentType }, ctx: RootCtx): void => {
  const children = [fiber.type(fiber.props)]
  reconcileChildren(fiber, children, ctx)
}

const performUnitOfWork = (fiber: Fiber, ctx: RootCtx): Fiber | undefined => {
  workingFiber = fiber

  if (typeof fiber.type === "string") {
    updateHostComponent(fiber as Fiber & { type: string }, ctx)
  } else if (typeof fiber.type === "function") {
    updateFnComponent(fiber as Fiber & { type: FnComponentType }, ctx)
  } else {
    console.error("Invalid element type", elementToName(fiber), fiber)
    throw new Error()
  }

  if (fiber.child != null) {
    return fiber.child
  }

  let nextFiber: Fiber | undefined = fiber
  while (nextFiber != null) {
    if (nextFiber.sibling != null) {
      return nextFiber.sibling
    }
    nextFiber = nextFiber.parent
  }
  return undefined
}

const render = (element: Element, ctx: RootCtx): void => {
  const fiber: Fiber = {
    phase: "before",
    type: "__root" as any, // 特殊なタイプ
    props: {
      children: [element],
    },
    alternate: ctx.currentRoot,

    hooks: [],
    hookIndex: 0,
  }

  ctx.nextUnitOfWork = fiber
  ctx.wipRoot = fiber
}

const rerender = (ctx: RootCtx): void => {
  const currentRoot = ctx.currentRoot
  if (currentRoot == null) throw new Error("can't re-render before initial rendering")

  const fiber: Fiber = {
    phase: "before",
    type: currentRoot.type,
    props: currentRoot.props,
    alternate: currentRoot,
    hooks: [],
    hookIndex: 0,
  }

  ctx.nextUnitOfWork = fiber
  ctx.wipRoot = fiber
  // ctx.deletions = []
}

// -----------------------------------------------
// commit
// -----------------------------------------------

let commitCount = 0

/**
 * 完成したファイバーツリーを再帰的に辿って構造を出力する。
 */
const commitRoot = (ctx: RootCtx) => {
  const go = (fiber: Fiber | undefined): void => {
    if (fiber == null) return

    switch (fiber.effectTag!) {
      case "UPDATE": {
        TRACE && console.log("enter", elementToName(fiber))

        const alternate = fiber.alternate!
        if (fiber.type === "text") {
          const wipValue = fiber.props.value
          const oldValue = alternate.props.value
          if (wipValue !== oldValue) {
            console.log("SET", wipValue)
          }
        }
        break
      }
      case "PLACEMENT": {
        console.log("BEGIN", elementToName(fiber))
        if (fiber.type === "text") {
          const value = fiber.props.value
          console.log("text", value)
        }
        break
      }
      case "DELETION":
        console.error("delete fiber in wipRoot?", fiber)
        return

      default:
        console.error("no effect tag?", fiber)
        return
    }

    updateEffects(fiber)
    go(fiber.child)

    switch (fiber.effectTag!) {
      case "UPDATE": {
        TRACE && console.log("leave", elementToName(fiber))
        break
      }
      case "PLACEMENT": {
        console.log("END", elementToName(fiber))
        break
      }
      default: throw never(fiber.effectTag)
    }

    go(fiber.sibling)
  }

  const goDelete = (fiber: Fiber): void => {
    console.log("DELETE", elementToName(fiber), fiber?.props?.value)

    const onDelete = (fiber: Fiber | undefined): void => {
      if (fiber == null) return
      onDelete(fiber.child)
      disposeEffects(fiber)
      onDelete(fiber.sibling)
    }
    onDelete(fiber)
  }

  commitCount++
  TRACE && console.log("commitRoot", `#${commitCount}`, "delete:", ctx.deletions.length)
  for (const deletedFiber of ctx.deletions.splice(0, ctx.deletions.length)) {
    goDelete(deletedFiber)
  }

  if (ctx.wipRoot != null) {
    go(ctx.wipRoot.child)
    ctx.currentRoot = ctx.wipRoot
    ctx.wipRoot = undefined
  }
}

/**
 * 処理を進める。
 *
 * - レンダリングやコミットの処理を行う。
 * - `host.interrupted` がシグナルしたら中断する。
 * - この関数は自身の再実行を登録するので、一度呼んだら自動で呼ばれ続ける。
 */
const workLoop = (ctx: RootCtx) => {
  workingRoot = ctx

  TRACE && console.log("working...")
  while (ctx.nextUnitOfWork != null && !ctx.interrupted) {
    ctx.nextUnitOfWork = performUnitOfWork(ctx.nextUnitOfWork, ctx)
  }

  if (ctx.nextUnitOfWork == null && ctx.wipRoot != null && !ctx.interrupted) {
    commitRoot(ctx)
  }

  ctx.requestIdleCallback(() => workLoop(ctx))
}

// -----------------------------------------------
// ルート
// -----------------------------------------------

export interface Root {
  render(element: Element): void
}

export const createRoot = (_container: unknown, host: RootHost): Root => {
  const ctx: RootCtx = {
    get interrupted() {
      return host.interrupted
    },
    requestIdleCallback(action: () => void) {
      host.requestIdleCallback(action)
    },
    deletions: [],
  }

  let running = false

  return {
    render: element => {
      render(element, ctx)

      if (!running) {
        running = true
        workLoop(ctx)
      }
    },
  }
}

export const nodeToString = (root: Element): string => {
  let s = ""
  let depth = 0
  const indent = () => "  ".repeat(depth)

  const go = (element: Element): void => {
    if (typeof element.type === "string") {
      s += "<"
      s += element.type

      const { children, ...props } = element.props

      for (const [key, value] of Object.entries(props)) {
        s += " "
        s += key
        s += "=\""
        s += value
        s += "\""
      }

      if (children == null || children.every(c => c == null)) {
        s += " />"
        return
      }
      s += ">"

      depth++
      for (const child of children) {
        s += "\n"
        s += indent()
        go(child)
      }
      depth--

      s += "\n"
      s += indent()
      s += "</"
      s += element.type
      s += ">"
    } else {
      s += "<Component... />"
    }
  }

  go(root)
  return s
}

const flatMap = <T, U>(iterable: Iterable<T>, f: (item: T, index: number) => Iterable<U>): U[] => {
  const result: U[] = []
  let i = 0
  for (const x of iterable) {
    result.push(...f(x, i))
    i++
  }
  return result
}

const never = (never: never): never => never
