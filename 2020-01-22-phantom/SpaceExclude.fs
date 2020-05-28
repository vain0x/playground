module rec SpaceExclude

open Core

let optMin first second =
  match first, second with
  | Some x, Some y ->
    min x y |> Some

  | _, None ->
    first

  | None, _ ->
    second

let optMax first second =
  match first, second with
  | Some x, Some y ->
    max x y |> Some

  | _, None ->
    first

  | None, _ ->
    second

let rec spaceExclude first second =
  match first, second with
  | UnionSpace [], _ ->
    spaceEmpty

  | _, UnionSpace [] ->
    first

  | IntRangeSpace (l, r),
    IntRangeSpace (xl, xr) ->
    //   <----- [l --------------- r) --------->
    // \ <----------- [xl -- xr) -------------->
    //   <----- [l --------------- r) --------->
    // = <------[---- xl) + [xr -- r) --------->

    spaceUnion [
      match xl with
      | Some _ ->
        IntRangeSpace (l, xl)

      | None ->
        ()

      match xr with
      | Some _ ->
        IntRangeSpace (xr, r)

      | None ->
        ()
    ]

  | IntRangeSpace _, TySpace I64Ty ->
    spaceEmpty
    
  | _, UnionSpace seconds ->
    seconds |> List.fold spaceExclude first

  | UnionSpace firsts, _ ->
    firsts |> List.map (fun first -> spaceExclude first second) |> spaceUnion

  | TySpace first, TySpace second
    when tyIsSubtypeOf first second ->
    spaceEmpty

  | _ ->
    match first |> spaceTryDecompose with
    | Some first ->
      spaceExclude first second 
    
    | None ->

    match second |> spaceTryDecompose with
    | Some second ->
      spaceExclude first second

    | None ->
      first

let spaceIntersect first second =
  spaceExclude first (spaceExclude first second)
