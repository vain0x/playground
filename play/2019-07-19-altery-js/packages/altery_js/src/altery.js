import { AlteryFnSpace } from "./functions"
import pat from "./patterns"

const DEF_HANDLER = {
  // def.prop(...patterns)(binding => result)
  get: (target, prop) => {
    const fn = target.get(prop)
    return (...patterns) => body =>
      fn.def(pat.of(patterns), body)
  },
}

const FN_HANDLER = {
  // fn.prop(...args)
  get: (target, prop) => {
    const fn = target.get(prop)
    return (...args) => fn.call(args)
  },
}

const FN_SPACE = new AlteryFnSpace()

const DEF_PROXY = new Proxy(FN_SPACE, DEF_HANDLER)

const FN_PROXY = new Proxy(FN_SPACE, FN_HANDLER)

export default {
  def: DEF_PROXY,
  fn: FN_PROXY,
  pat,
}
