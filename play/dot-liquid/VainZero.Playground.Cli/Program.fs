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

module LiquidExtension =
  open DotLiquid

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

  let render source kvs =
    let template = Template.Parse(source)
    template.Render(Hash.FromDictionary(dict kvs))

module Program =

  [<EntryPoint>]
  let main argv =
    let (-->) k v = (k, box v)
    let dir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let source = File.ReadAllText(Path.Combine(dir, "hello.liquid"))

    // Enable automatic HTML encoding.
    DotLiquid.Template.RegisterValueTypeTransformer(typeof<string>, fun m -> WebUtility.HtmlEncode(m :?> string) |> box)

    let model = ["name" --> "<script>vain0</script>"]
    let target = LiquidExtension.render source model
    printfn "%s" target
    0
