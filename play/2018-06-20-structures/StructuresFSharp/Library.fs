namespace StructuresFSharp

// https://gist.github.com/pchiusano/ba958c70ebaf535aa805

type V2 =
  {
    X: int
    Y: int
  }

module V2 =
  let zero = { X = 0; Y = 0 }

type Region =
  {
    TopLeft: V2
    Width: int
    Height: int
  }

module Element =
  type T =
    private
    | Empty
    | EBeside
      of T * T
    | EAbove
      of T * T
    | EContainer
      of Region * T

  let empty =
    T.Empty

  let beside left right =
    T.EBeside (left, right)

  let above top bottom =
    T.EAbove (top, bottom)

  let container width height pos element =
    let region = { TopLeft = pos; Width = width; Height = height }
    EContainer (region, element)

  let rec widthOf =
    function
    | Empty -> 0
    | EBeside (left, right) ->
      widthOf left + widthOf right
    | EAbove (top, bottom) ->
      max (widthOf top) (widthOf bottom)
    | EContainer ({ Width = width }, _) ->
      width

  let rec heightOf =
    function
    | Empty -> 0
    | EBeside (left, right) ->
      max (heightOf left) (heightOf right)
    | EAbove (top, bottom) ->
      heightOf top + heightOf bottom
    | EContainer ({ Height = height }, _) ->
      height

  let absolute =
    ()

  let rec topLeftAt =
    function
    | Empty -> V2.zero
    | EBeside (left, _) ->
      topLeftAt left
    | EAbove (top, _) ->
      topLeftAt top
    | EContainer ({ TopLeft = topLeft }, _) ->
      topLeft

type LayoutF<'R> =
  | Embed
    of Element
  | Container
    of Region * 'R
  | Beside
    of 'R * 'R
  | Above
    of 'R * 'R

and Layout<'K> =
  | Layout
    of LayoutF<Layout<'K>> * Element * 'K



module Say =
    let hello name =
        printfn "Hello %s" name
