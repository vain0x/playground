namespace VainZero.Playground

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Net
open System.Reflection
open System.Web
open System.Text
open System.Threading
open System.Runtime.InteropServices
open DotLiquid

module LiquidExtension =
  open DotLiquid

  (*
  type HtmlBlock() =
    inherit Block()

    let _escapeFilter =
      Variable.Filter("escape", arguments = [||])

    static do
      //Template.RegisterTag<HtmlBlock>("html")
      ()

    override this.Render(context: Context, writer: TextWriter): unit =
      for node in this.NodeList do
        match node with
        | :? Variable as var when var.Filters.Count = 0 ->
          var.Filters.Add(_escapeFilter)
        | _ -> ()
      base.Render(context, writer)
  *)

  let hasify =
    let rec go (model: obj) =
      match model with
      | null
      | :? string
      | :? int
      | :? int64
      | :? float
      | :? decimal ->
        model
      | _ ->
        hasify model :> obj
    and hasify model =
      let dict =
        Hash.FromAnonymousObject(model)
        |> Seq.map (fun (KeyValue (key, value)) -> (key, go value))
        |> dict
      Hash.FromDictionary(dict)
    hasify

  let render source (model: Hash) =
    let template = Template.Parse(source)
    template.Render(model)

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
  let main argv =
    let (-->) k v = (k, box v)
    // let dir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    // let source = File.ReadAllText(Path.Combine(dir, "hello.liquid"))

    // Enable automatic HTML encoding.
    DotLiquid.Template.RegisterValueTypeTransformer(typeof<string>, fun m -> WebUtility.HtmlEncode(m :?> string) |> box)

    let source = File.ReadAllText("hello.liquid.html")
    let model = { Message = { Text = "<script>John Doe</script>" } }
    // let model = new DotLiquid.Hash()
    // let message = new DotLiquid.Hash()
    // message.Add("Screem", "OK!")
    // message.Add("Silent", "yes...")
    // model.Add("Message", message)
    let target = LiquidExtension.render source (model |> LiquidExtension.hasify)
    printfn "%s" target
    0
