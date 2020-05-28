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

class QuzieCollection {
  constructor(parent) {
    this.diff = []
    this.parent = parent
  }

  drainDiff() {
    return this.diff.splice(0, this.diff.length)
  }

}

class QuzieArray extends QuzieCollection {
  constructor(parent) {
    super(parent)

    this.inner = []
  }

  get size() {
    return this.inner.length
  }

  push(value) {
    const index = this.inner.length

    this.inner.push(value)
    this.diff.push(["D_ARRAY_INSERT", index, value])
    this.parent.mark()
  }
}

class QuzieObject extends QuzieCollection {
  constructor(parent) {
    super(parent)

    this.inner = new Map()
  }

  get size() {
    return this.inner.size
  }

  set(key, value) {
    const current = this.inner.get(key)
    if (current === value) {
      return
    }

    const delta = current ? "D_OBJECT_SET" : "D_OBJECT_ADD"
    this.diff.push([delta, index, value])
    this.inner.set(key, value)

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

  newBool(value) {
    return new QuzieConstRef(value)
  }

  newString(value) {
    return new QuzieConstRef(value)
  }

  newArray() {
    return new QuzieConstRef(new QuzieArray(QUZIE_ROOT))
  }

  newObject() {
    return new QuzieConstRef(new QuzieObject(QUZIE_ROOT))
  }

  newLocal(name, valueRef) {
    const local = new QuzieLocal(name)
    this.setValue(local, valueRef)
    return local
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
      if (value instanceof QuzieCollection) {
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

  setEntry(objectRef, keyRef, valueRef) {
    const object = objectRef.compute()
    if (!(object instanceof QuzieObject)) {
      throw new Error("type error")
    }

    const key = keyRef.compute()
    const value = valueRef.compute()

    object.set(key, value)
    this.didUpdate()
  }
}
