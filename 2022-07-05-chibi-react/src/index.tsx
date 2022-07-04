import * as Didact from "./didact"

const range = (n: number) => [...new Array(n).keys()]

// console.log(Didact.nodeToString((
//   <>
//     <text value="a">
//       hello, world!
//     </text>
//     <text value="other" />
//   </>
// )))

const App = (props: { msg: string, count: number }) => {
  const { msg, count } = props
  console.log("trace:", "App", msg, count)

  return (
    <>
      {msg}

      {count !== 0 ? <List count={count} /> : null}

      <text value="last" />
    </>
  )
}

const List = (props: { count: number }) => {
  const { count } = props
  console.log("trace:", "List", count)

  Didact.useEffect(() => {
    console.log("effect: count is now", count)
    return () => {
      console.log("effect: count changed or list is disposed?", count)
    }
  }, [count])

  const [delayed, setDelayed] = Didact.useState(false)
  Didact.useEffect(() => {
    const h = setTimeout(() => {
      if (!delayed) {
        setDelayed(true)
      }
    }, 1)
    return () => clearTimeout(h)
  }, [])

  return (
    <div>
      List:
      {range(count).map(i => (
        <text key={i} value={`Item ${i + 1}`} />
      ))}

      {delayed ? "delayed!" : null}
    </div>
  )
}

const call = (f: () => void) => f()
const delay = () => new Promise<void>(resolve => setTimeout(() => resolve(), 100))

call(async () => {
  const host: Didact.RootHost = {
    interrupted: false,
    requestIdleCallback: action => {
      setTimeout(action, 1)
    },
  }
  const root = Didact.createRoot(null, host)

  root.render((<App msg="Hello, world!" count={2} />))
  await delay()
  console.log("------------------")
  root.render((<App msg="Hello, world!" count={2} />))
  await delay()
  console.log("------------------")
  root.render((<App msg="Hello, world!" count={3} />))
  await delay()
  console.log("------------------")
  root.render((<App msg="Good bye!" count={0} />))
  await delay()

  // Shutdown.
  host.interrupted = true
  host.requestIdleCallback = () => undefined
})
