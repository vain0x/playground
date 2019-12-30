class QuzieRoot {
  get dirty() {
    return true
  }

  compute() {
    return null
  }

  mark() {
  }
}

const QUZIE_ROOT = new QuzieRoot()

class QuzieLocal {
  constructor(name) {
    this.name = name
    this.value = null
  }

  get dirty() {
    return true
  }

  compute() {
    return this.value
  }

  mark() {
  }

  setValue(value) {
    if (value instanceof QuzieArray) {
      value.parent = this
    }
    this.value = value
  }
}

class QuzieConstRef {
  constructor(value) {
    this.value = value
  }

  get dirty() {
    return false
  }

  compute() {
    return this.value
  }

  mark() {
    throw new Error("literal can't be modified")
  }
}

class QuzieArray {
  constructor(parent) {
    this.inner = []
    this.diff = []
    this.parent = parent
  }

  get length() {
    return this.inner.length
  }

  drainDiff() {
    return this.diff.splice(0, this.diff.length)
  }

  push(value) {
    const index = this.inner.length

    this.inner.push(value)
    this.diff.push(["D_ARRAY_INSERT", index, value])
    this.parent.mark()
  }
}

class QuzieQuery {
  constructor(valueRef, computeFun) {
    this.valueRef = valueRef
    this.computeFun = computeFun
  }

  compute() {

  }
}

export class QuzieRuntime {
  constructor() {
    this.subscriptions = []
  }

  newString(value) {
    return new QuzieConstRef(value)
  }

  newArray() {
    return new QuzieConstRef(new QuzieArray(QUZIE_ROOT))
  }

  newLocal(name) {
    return new QuzieLocal(name)
  }

  subscribe(valueRef, callback) {
    this.subscriptions.push({ valueRef, callback })
  }

  didUpdate() {
    for (const { valueRef, callback } of this.subscriptions) {
      if (!valueRef.dirty) {
        return
      }

      const value = valueRef.compute()
      if (value instanceof QuzieArray) {
        for (const delta of value.drainDiff()) {
          callback(delta)
        }
        return
      }

      callback(value)
    }
  }

  setValue(targetRef, valueRef) {
    targetRef.setValue(valueRef.compute())
    this.didUpdate()
  }

  addItem(arrayRef, valueRef) {
    const array = arrayRef.compute()
    if (!(array instanceof QuzieArray)) {
      throw new Error("type error")
    }

    const value = valueRef.compute()

    array.push(value)
    this.didUpdate()
  }
}
