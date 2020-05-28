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

const INT = Symbol("int")

const OP_ADD = Symbol("+")

const Int = value => [INT, value]

const Add = (left, right) => [OP_ADD, left, right]

const node = Add(Add(Int(2), Int(3)), Int(5))

A.def.eval(Int(X))(({ X }) => X)

A.def.eval(Add(X, Y))(({ X, Y }) => A.fn.eval(X) + A.fn.eval(Y))

is(A.fn.eval(node), 2 + 3 + 5)
