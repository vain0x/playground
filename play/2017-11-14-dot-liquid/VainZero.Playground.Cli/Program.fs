namespace VainZero.Playground

open System
open System.Collections
open System.IO
open System.Net
open System.Text.RegularExpressions
open DotLiquid

module LiquidExtension =

  // roughly
  let private deepHasify =
    let rec go (model: obj) =
      match model with
      | null
      | :? string
      | :? int
      | :? int64
      | :? float
      | :? decimal
      | :? DateTime
      | :? TimeSpan ->
        model
      | :? IDictionary ->
        NotImplementedException("TODO: Impl dictionary hasify") |> raise
      | :? ICollection as xs ->
        [| for x in xs -> go x |] :> obj
      | _ ->
        hasify model :> obj
    and hasify model =
      model
      |> fun m -> Hash.FromAnonymousObject(m)
      |> Seq.map (fun (KeyValue (key, value)) -> (key, go value))
      |> dict
      |> fun d -> Hash.FromDictionary(d)
    hasify

  let setUp () =
    // Enable automatic HTML encoding.
    Template.RegisterValueTypeTransformer(typeof<string>, fun m -> WebUtility.HtmlEncode(m :?> string) |> box)

  let build<'T> templateSource =
    let template = lazy Template.Parse(templateSource)
    fun (model: 'T) ->
      let hash = deepHasify model
      template.Value.Render(hash)

module Program =

  type Message =
    {
      Text: string
    }
  with
    member this.Screem = this.Text.ToUpper() + "!"

    member this.Silent = "(" + this.Text.ToLower() + ")"

  type Model =
    {
      Message: Message
    }

  [<EntryPoint>]
  let main _ =
    let (-->) k v = (k, box v)

    LiquidExtension.setUp ()

    let source = File.ReadAllText("hello.liquid.html")
    let render: Model -> string = LiquidExtension.build source

    let model =
      {
        // XSS attack!
        Message = { Text = "<script>John Doe</script>" }
      }
    let target = render model

    printfn "%s" target

    assert (target.Contains("&lt;/SCRIPT&gt;"))
    0
