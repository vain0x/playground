import { PATTERN_MATCH } from "./patterns"

const EMPTY_BINDING = {}

export class AlteryMatchExhaustError extends Error {
  constructor(value, key) {
    super("Match exhaust")

    this.value = value
    this.key = key
  }
}

class AlteryMatchArm {
  constructor(pattern, body) {
    this._pattern = pattern
    this._body = body
  }

  match(arg) {
    return this._pattern[PATTERN_MATCH](arg, EMPTY_BINDING)
  }

  call(binding) {
    return this._body(binding)
  }
}

class AlteryFn {
  constructor(name, arms) {
    this._name = name
    this._arms = arms
  }

  isUndefined() {
    return this._arms.length === 0
  }

  def(pattern, body) {
    this._arms.push(new AlteryMatchArm(pattern, body))
  }

  call(arg) {
    for (const arm of this._arms) {
      const binding = arm.match(arg)
      if (binding === null) {
        continue
      }

      return arm.call(binding)
    }

    throw new AlteryMatchExhaustError(arg, this._name)
  }
}

export class AlteryFnSpace {
  _map = new Map()

  get(name) {
    let fn = this._map.get(name)
    if (!fn) {
      fn = new AlteryFn(name, [])
      this._map.set(name, fn)
    }
    return fn
  }

  call(name, arg) {
    const fn = this.get(name)

    if (fn.isUndefined()) {
      throw new AlteryMatchExhaustError(arg, name)
    }

    return fn.call(arg)
  }

  def(name, pattern, body) {
    const fn = this.get(name)
    fn.def(pattern, body)
  }
}
