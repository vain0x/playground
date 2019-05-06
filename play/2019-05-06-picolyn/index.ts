import { Event } from "./types"
import { GreenNodeBuilder } from "./green"
import { exhaust } from "./util"
import { redNodeRootFromGreenNode } from "./red"

const build = (events: Event[]) => {
  const builder = new GreenNodeBuilder()
  for (const event of events) {
    if (event.type === "token") {
      builder.leaf(event.kind, event.text)
      continue
    }
    if (event.type === "start") {
      builder.startNode(event.kind)
      continue
    }
    if (event.type === "finish") {
      builder.finishNode()
      continue
    }
    throw exhaust(event)
  }
  return builder.finish()
}

const main = () => {
  // 1+2
  const events: Event[] = [
    { type: "start", kind: "root" },
    { type: "start", kind: "bin" },
    { type: "start", kind: "literal" },
    { type: "token", kind: "int", text: "2" },
    { type: "finish" },
    { type: "token", kind: "plus", text: "+" },
    { type: "start", kind: "literal" },
    { type: "token", kind: "int", text: "3" },
    { type: "finish" },
    { type: "finish" },
    { type: "finish" }
  ]
  const green = build(events)
  const root = redNodeRootFromGreenNode(green)
  console.log(root)
}

main()
