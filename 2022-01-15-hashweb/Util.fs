[<AutoOpen>]
module HashWeb.Util

module String =
  let startsWith (prefix: string) (s: string) = s.StartsWith(prefix)
  let endsWith (prefix: string) (s: string) = s.EndsWith(prefix)
  let contains (infix: string) (s: string) = s.Contains(infix)
  let replace (infix: string) (subst: string) (s: string) = s.Replace(infix, subst)
  let toLower (s: string) = s.ToLowerInvariant()
