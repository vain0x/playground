type ActionFn<T> = (item: T, index: number) => void

export abstract class Loop<T> {
  abstract run(action: ActionFn<T>): void

  forEach(action: ActionFn<T>): void {
    this.run(action)
  }

  map<U>(mapping: (item: T, index: number) => U): Loop<U> {
    return new MapLoop<T, U>(this, mapping)
  }

  static range(start: number, end: number): Loop<number> {
    return new RangeLoop(start, end)
  }
}

class RangeLoop extends Loop<number> {
  constructor(private readonly start: number, private readonly end: number) {
    super()
  }

  run(action: ActionFn<number>): void {
    for (let i = this.start; i < this.end; i++) {
      action(i, i - this.start)
    }
  }
}

class MapLoop<T, U> extends Loop<U> {
  constructor(
    private readonly source: Loop<T>,
    private readonly mapping: (item: T, index: number) => U,
  ) {
    super()
  }

  override run(action: (item: U, index: number) => void): void {
    this.source.run((item, index) => {
      action(this.mapping(item, index), index)
    })
  }
}
