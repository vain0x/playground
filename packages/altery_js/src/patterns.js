//! Pattern matchers

export const PATTERN_MATCH = Symbol("Pattern.match")

export class Pattern {
  notNull() {
    return new NotNullPattern(this)
  }

  when(pred) {
    return new WhenPattern(pred, this)
  }

  and(second) {
    return new AndPattern(this, second)
  }

  [PATTERN_MATCH](_target, _binding) {
    throw new Error("unimplemented")
  }
}

class DiscardPattern extends Pattern {
  [PATTERN_MATCH](_target, binding) {
    return binding
  }
}

class VarPattern extends Pattern {
  constructor(name) {
    super()
    this._name = name
  }

  [PATTERN_MATCH](target, binding) {
    return { ...binding, [this._name]: target }
  }
}

class NullPattern extends Pattern {
  [PATTERN_MATCH](target, binding) {
    return target == null ? binding : null
  }
}

class NotNullPattern extends Pattern {
  constructor(inner) {
    super()
    this._inner = inner
  }

  [PATTERN_MATCH](target, binding) {
    return target != null
      ? this._inner[PATTERN_MATCH](target, binding)
      : null
  }
}

class StrictEqualPattern extends Pattern {
  constructor(value) {
    super()
    this._value = value
  }

  [PATTERN_MATCH](target, binding) {
    return target === this._value
      ? binding
      : null
  }
}

class WhenPattern extends Pattern {
  constructor(pred, inner) {
    super()
    this._pred = pred
    this._inner = inner
  }

  [PATTERN_MATCH](target, binding) {
    return this._pred(target)
      ? this._inner[PATTERN_MATCH](target, binding)
      : null
  }
}

class AndPattern extends Pattern {
  constructor(first, second) {
    super()
    this._first = first
    this._second = second
  }

  [PATTERN_MATCH](target, binding) {
    binding = this._first[PATTERN_MATCH](target, binding)
    if (!binding) {
      return null
    }

    return this._second[PATTERN_MATCH](target, binding)
  }
}

class ArrayPattern extends Pattern {
  constructor(patterns) {
    super()
    this._patterns = patterns
  }

  [PATTERN_MATCH](target, binding) {
    if (!Array.isArray(target)) {
      return null
    }

    if (target.length !== this._patterns.length) {
      return null
    }

    for (let i = 0; i < this._patterns.length; i++) {
      const pattern = this._patterns[i]

      binding = pattern[PATTERN_MATCH](target[i], binding)
      if (!binding) {
        return null
      }
    }

    return binding
  }
}

const DISCARD_PATTERN = new DiscardPattern()

const NULL_PATTERN = new NullPattern()

const FALSE_PATTERN = new StrictEqualPattern(false)

const TRUE_PATTERN = new StrictEqualPattern(true)

const ZERO_PATTERN = new StrictEqualPattern(0)

const EMPTY_STRING_PATTERN = new StrictEqualPattern("")

const UNIT_PATTERN = new ArrayPattern([])

const VAR_CACHE = new Map([["_", DISCARD_PATTERN]])

const varPattern = key => {
  let pattern = VAR_CACHE.get(key)
  if (!pattern) {
    pattern = new VarPattern(key)
    VAR_CACHE.set(key, pattern)
  }
  return pattern
}

const stringPattern = value => {
  if (value === "") {
    return EMPTY_STRING_PATTERN
  }

  return new StrictEqualPattern(value)
}

const patternFrom = obj => {
  if (typeof obj === "object") {
    if (obj === null) {
      return NULL_PATTERN
    }

    if (PATTERN_MATCH in obj) {
      return obj
    }

    if (Array.isArray(obj)) {
      if (obj.length === 0) {
        return UNIT_PATTERN
      }

      return new ArrayPattern(obj.map(patternFrom))
    }

    throw new Error("FIXME: object pattern not implemented yet")
  }

  if (obj === true) {
    return TRUE_PATTERN
  }

  if (obj === false) {
    return FALSE_PATTERN
  }

  if (typeof obj === "string") {
    return varPattern(obj)
  }

  if (typeof obj === "number") {
    if (Number.isNaN(obj)) {
      throw new Error("FIXME: NaN pattern not implemented yet")
    }

    if (obj === 0) {
      return ZERO_PATTERN
    }

    return new StrictEqualPattern(obj)
  }

  if (typeof obj === "undefined") {
    return NULL_PATTERN
  }

  throw new Error("Unknown pattern object")
}

export default {
  _:
    DISCARD_PATTERN,
  self:
    varPattern("self"),
  var:
    varPattern,
  null:
    NULL_PATTERN,
  notNull:
    DISCARD_PATTERN.notNull(),
  str:
    stringPattern,
  of:
    patternFrom,
}
