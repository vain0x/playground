import { h } from "hyperapp"
import { Program, UpdateFn } from "../tea/types"
import { cmdNone } from "../tea/prelude"

const exhaust = (value: never) => value

interface Model {
  count: number,
}

interface MsgIncrement {
  type: "INCREMENT",
}

type Msg = MsgIncrement

const mkMsg = (msg: Msg) => msg

const update: UpdateFn<Model, Msg> = (model, msg) => {
  switch (msg.type) {
    case "INCREMENT": {
      return [{ count: model.count + 1 }, cmdNone]
    }
    default: {
      throw exhaust(msg.type)
    }
  }
}

const view = (model: Model) => {
  return (
    <main>
      <div>
        Count: <code>{model.count}</code>
      </div>

      <div>
        <button type="button" data-onclick={mkMsg({ type: "INCREMENT" })}>
          +
        </button>
      </div>
    </main>
  )
}

const init: Program<Model, Msg> = {
  model: {
    count: 0,
  },
  cmd: cmdNone,
  update,
  view,
}

export default init
