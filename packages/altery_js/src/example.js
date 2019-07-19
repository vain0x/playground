import { deepStrictEqual as is } from "assert"
import A from "./altery"

const P = A.pat

const X = P.var("X")

const Y = P.var("Y")

A.def.append(X.notNull(), Y.notNull())(({ X, Y }) => X + Y)

A.def.append(null, X.notNull())(({ X }) => X)

A.def.append(X.notNull(), null)(({ X }) => X)

A.def.append(null, null)(() => "")

is(A.fn.append("hello, ", "world"), "hello, world")

is(A.fn.append(null, "bye"), "bye")

is(A.fn.append(null, null), "")
