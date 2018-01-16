import * as Q from "typescript";
import * as E from "linq";

// const t = HTMLElement;
const f = Array.from;

const a = async () => {
  return 1;
};

export const write = (p: (x: number) => void) => E.range(1, 10).toArray().forEach(x => p(x));


export { a, f };
