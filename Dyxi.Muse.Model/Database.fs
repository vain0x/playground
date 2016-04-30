[<AutoOpen>]
module Dyxi.Muse.Model.Database

open FSharp.Data.Sql

type sql =
  SqlDataProvider<
      Common.DatabaseProviderTypes.MYSQL
    , ConnectionString  = "Data Source=localhost; user=root; password=;"
    , UseOptionTypes    = true
    , Owner = "dyxi_muse"
    >

let ctx = sql.GetDataContext()
let db = ctx.DyxiMuse
