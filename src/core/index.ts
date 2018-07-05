
interface VarPattern {
  var: string;
}

type Pattern = VarPattern;

interface LetExpr {
  let: Pattern;
  be: Expr;
  in: Expr;
}

interface DoExpr {
  do: Expr;
  in: Expr;
}

interface EffectExpr {
  effect: Expr;
  body: Expr;
}

interface AffectExpr {
  affect: Expr;
}

interface BinaryExpr {
  binary: string;
  left: Expr;
  right: Expr;
}

interface LiteralExpr {
  literal: string | number;
}

interface RefExpr {
  ref: string;
}

interface CallExpr {
  call: Expr;
  args: Expr[];
}

type Expr =
  | LiteralExpr
  | RefExpr
  | BinaryExpr
  | CallExpr
  | EffectExpr
  | AffectExpr
  | LetExpr;

interface IOAction {
  io: () => Value
}

interface IOContext {
  actions: IOAction[]
}

const ioEffect = {
  inject: (action: IOAction): IOContext =>
    ({ actions: [action] }),
  start: (): IOContext =>
    ({ actions: [] }),
  affect: (context: IOContext, action: IOAction) =>
    ({ actions: [...context.actions, action] })
};

type Ident = string;

type Value = string | number | object | IOAction;

interface Env {
  parent: Env | undefined;
  bindings: Map<Ident, Value>;
}

const resolveRef = (env: Env, ref: string): Value => {
  const value = env.bindings.get(ref);
  if (value !== undefined) {
    return value;
  }
  if (env.parent === undefined) {
    throw new Error(`Undefined ${ref}`);
  }
  return resolveRef(env.parent, ref);
};

const bind = (env: Env, ref: string, value: Value): Env => {
  const bindings = new Map<Ident, Value>([...env.bindings]);
  bindings.set(ref, value);
  return { parent: env.parent, bindings };
}

const jsnow: IOAction = {
  io() {
    return Date();
  }
};

const jslog = (value: Value): IOAction => ({
  io() {
    console.log(value);
    return { consoleLogResult: value };
  }
});

const exhaust = (x: never): never => x;

const evaluateBinOp = (op: string, left: Value, right: Value) => {
  const l = left as any, r = right as any;
  if (op === "+") {
    return (l + r) as Value;
  } else if (op === "*") {
    return (l * r) as Value;
  } else {
    throw new Error(`Unknown binary operator ${op}`);
  }
};

const evaluate = (expr: Expr, env: Env): Value => {
  if ("literal" in expr) {
    return expr.literal;
  } else if ("ref" in expr) {
    return resolveRef(env, expr.ref);
  } else if ("binary" in expr) {
    const left = evaluate(expr.left, env);
    const right = evaluate(expr.right, env);
    return evaluateBinOp(expr.binary, left, right);
  } else if ("call" in expr) {
    if ("ref" in expr.call && expr.call.ref === "jslog" && expr.args.length === 1) {
      const arg = evaluate(expr.args[0], env);
      return jslog(arg);
    }
    throw new Error("not impl");
  } else if ("let" in expr) {
    const content = evaluate(expr.be, env);
    const nextEnv = bind(env, expr.let.var, content);
    return evaluate(expr.in, nextEnv);
  } else if ("affect" in expr) {
    const action = evaluate(expr.affect, env);
    // only in io context:
    if (typeof action === "object" && "io" in action) {
      return action.io();
    }
    throw new Error("Expected IO action");
  } else if ("effect" in expr) {
    return { io: () => evaluate(expr.body, env) };
  } else {
    return exhaust(expr);
  }
};

// let main = io {
//   let now = jsnow()!
//   let message = "It's " + now
//   jslog(now)!
// }
const sample: Expr = {
  let: {
    var: "main",
  },
  be: {
    effect: {
      ref: "io",
    },
    body: {
      let: { var: "now" },
      be: {
        affect: { ref: "jsnow" },
      },
      in: {
        let: { var: "message" },
        be: {
          binary: "+",
          left: { literal: "At " },
          right: { ref: "now" }
        },
        in: {
          affect: {
            call: { ref: "jslog" },
            args: [{ ref: "message" }],
          },
        }
      }
    }
  },
  in: { ref: "main" },
};

const main = () => {
  const emptyEnv: Env = { parent: undefined, bindings: new Map<Ident, Value>() };
  const defaultEnv: Env = bind(emptyEnv, "jsnow", jsnow);

  const action = evaluate(sample, defaultEnv);
  console.info("evaluation completed.");

  if (typeof action === "object" && "io" in action) {
    const result = action.io();
    console.log(result);
  } else {
    throw new Error(`main must be an action`);
  }
};

main();
