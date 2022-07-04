declare module JSX {
  interface IntrinsicElements {
    text: {
      key?: unknown
      value?: string | number | boolean | null | undefined
    }
    div: {}
    fragment: {}
  }
}
