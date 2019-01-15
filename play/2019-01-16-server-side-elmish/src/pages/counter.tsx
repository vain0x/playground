import { h } from "hyperapp"

const exhaust = (value: never) => value

interface Model {
  count: number,
}

interface MsgIncrement {
  type: "INCREMENT",
}

type Msg = MsgIncrement

const mkMsg = (msg: Msg) => msg

const update = (model: Model, msg: Msg) => {
  switch (msg.type) {
    case "INCREMENT": {
      return { count: model.count + 1 }
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
        <button type="button" onclick={mkMsg({ type: "INCREMENT" })}>
          +
        </button>
      </div>
    </main>
  )
}

export default {
  model: {
    count: 0,
  },
  update,
  view,
}
