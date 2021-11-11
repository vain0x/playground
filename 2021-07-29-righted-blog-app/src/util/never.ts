const raise = (err: unknown) => {
  if (typeof err === "object" && err instanceof Error) {
    throw err
  } else {
    throw new Error(`${err}`)
  }
}

export const never = (never: never): never => never ?? raise("never")
