declare global {
  interface Array<T> {
    flatMap<U>(callback: (this: undefined, item: T) => Iterable<U>): U[];

    choose<U>(callback: (this: undefined, item: T) => U | undefined): U[];
  }
}

Array.prototype.flatMap = function <T, U>(this: T[], callback: (item: T) => Iterable<U>) {
  const us: U[] = [];
  for (const t of this) {
    for (const u of callback(t)) {
      us.push(u);
    }
  }
  return us;
};

Array.prototype.choose = function <T, U>(this: T[], callback: (item: T) => U | undefined) {
  const us: U[] = [];
  for (const t of this) {
    const u = callback(t);
    if (u === undefined) continue;
    us.push(u);
  }
  return us;
};

export default { };
