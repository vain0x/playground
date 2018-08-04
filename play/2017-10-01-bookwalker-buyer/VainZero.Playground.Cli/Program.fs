module BookwalkerCoinCalculator.Program

// App to calculate purchase plans to minimize remaining ephemeral coins

// Algorithm:
// Purchase most-coined book separately and repeatedly is theoretically optimal.
// Bruteforce for combination of books per purchase.

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

let negate x = -x

let separators n r =
  let rec loop i r acc =
    seq {
      if i = n then
        yield 0 :: List.rev (n :: acc)
      else
        if r >= 1 && i >= 1 then
          yield! loop (i + 1) (r - 1) (i :: acc)
        yield! loop (i + 1) r acc
    }
  loop 0 r []

/// Splits an array into non-empty, contiguous subsequences
/// at up to `r` boundaries.
let separate r xs =
  seq {
    let n = xs |> Array.length
    for r in 0..(min n r - 1) do
      for separators in separators n r ->
        [|
          for (l, r) in separators |> List.pairwise ->
            assert (0 <= l && l < r && r <= n)
            [|for i in l..(r - 1) -> xs.[i]|]
        |]
  }

module BookWalker =
  type MoneyAmount = float

  type Money =
    {
      Jpy: MoneyAmount
      Coin: MoneyAmount
    }
  with
    override this.ToString() =
      String.Format("({0:N0} JPY, {1:N0} C)", this.Jpy, this.Coin)

  type Rate = float

  type User =
    {
      TaxRate: Rate
      CoinRate: Rate
    }

  type Payment =
    Money

  type CheckedBook =
    {
      Title: string
      Price: MoneyAmount
      SaleCoinRate: Rate
    }

  module Money =
    let jpy money = money.Jpy
    let coin money = money.Coin

    let zero =
      {
        Jpy = 0.0
        Coin = 0.0
      }

    let add (second: Money) (first: Money) =
      {
        Jpy =
          first.Jpy + second.Jpy
        Coin =
          first.Coin + second.Coin
      }

    let sum moneys =
      moneys |> Seq.fold add zero

  module Wallet =
    /// Calculates the result of wallet after the specified payment.
    let pay (payment: Money) (wallet: Money) =
      let totalPrice =
        payment.Jpy |> negate
      let consumedCoin =
        wallet.Coin |> min totalPrice
      {
        Jpy =
          wallet.Jpy - (totalPrice - consumedCoin)
        Coin =
          wallet.Coin - consumedCoin + payment.Coin
      }

    /// Calculates the result of wallet after payments.
    let payMany payments wallet =
      payments |> Seq.fold (fun wallet payment -> pay payment wallet) wallet

  type Suggestion =
    {
      Id: int
      ResultWallet: Money
      Carts: CheckedBook[][]
    }
  with
    static member Create(id, wallet, carts) =
      {
        Id = id
        ResultWallet = wallet
        Carts = carts
      }

  module Suggestion =
    let id suggestion = suggestion.Id
    let resultWallet suggestion = suggestion.ResultWallet
    let carts suggestion = suggestion.Carts

    let purchaseCount suggestion =
      suggestion.Carts |> Array.length

    let isTrivial suggestion =
      suggestion |> purchaseCount <= 1

    let resultCoin suggestion =
      suggestion.ResultWallet |> Money.coin

  type SuggestionList =
    Suggestion[]

  module SuggestionList =
    /// Removes dominated suggestions and sorts in best-first order.
    let evaluate (suggestions: SuggestionList) =
      let goods =
        suggestions
        |> Seq.filter (Suggestion.isTrivial >> not)
        |> Seq.groupBy Suggestion.purchaseCount
        |> Seq.map (fun (_, g) -> g |> Seq.minBy Suggestion.resultCoin)
        |> Seq.sortBy Suggestion.resultCoin
        |> Seq.toArray
      let bads =
        suggestions
        |> Seq.except goods
        |> Seq.sortBy Suggestion.resultCoin
        |> Seq.toArray
      (goods, bads)

  type EvaluationContext =
    {
      User: User
      UserWallet: Money
      MaxPurchaseCount: int
    }

  type EvaluationResult =
    {
      Context: EvaluationContext
      BestWallet: Money
      GoodSuggestions: SuggestionList
      BadSuggestions: SuggestionList
    }

  module Logic =
    let paymentOnBook (user: User) (checkedBook: CheckedBook) =
      let price = checkedBook.Price
      let ephemeralCoin = price * checkedBook.SaleCoinRate
      let durableCoin = price * user.CoinRate
      {
        Jpy = -price * (user.TaxRate + 1.0)
        Coin = ephemeralCoin + durableCoin
      }

    let paymentOnCart user checkedBooks =
      checkedBooks
      |> Seq.map (fun b -> paymentOnBook user b)
      |> Money.sum

    let coinAmountOnPurchase user checkedBook =
      paymentOnBook user checkedBook
      |> Money.coin

    let suggest (context: EvaluationContext) (checkedBooks: CheckedBook[]) =
      let user = context.User
      let userWallet = context.UserWallet
      let checkedBooks =
        checkedBooks |> Array.sortByDescending (coinAmountOnPurchase user)
      let bestWallet =
        checkedBooks
        |> Seq.map (fun books -> paymentOnBook user books)
        |> fun payments -> userWallet |> Wallet.payMany payments
      let suggestions =
        seq {
          let splitBooksIntoCarts () =
            checkedBooks |> separate context.MaxPurchaseCount
          for carts in splitBooksIntoCarts () do
            let payments =
              carts |> Seq.map (fun books -> paymentOnCart user books)
            let wallet =
              userWallet |> Wallet.payMany payments
            yield (wallet, carts)
        }
        |> Seq.mapi (fun i (w, bss) -> Suggestion.Create(i, w, bss))
        |> Seq.toArray
      let (goods, bads) =
        suggestions |> SuggestionList.evaluate
      {
        Context = context
        BestWallet = bestWallet
        GoodSuggestions = goods
        BadSuggestions = bads
      }

    let display verbose (result: EvaluationResult) =
      let { User = user; UserWallet = userWallet } = result.Context

      let displayFull (suggestion: Suggestion) =
        printfn "- %d accounts Result=%O"
          (suggestion |> Suggestion.purchaseCount)
          suggestion.ResultWallet
        for (i, books) in suggestion.Carts |> Seq.indexed do
          printfn "  #%d account:" (i + 1)
          for book in books do
            printfn "    %s" book.Title

      let displayCompact (suggestion: Suggestion) =
        let strArray xs = "[" + (xs |> Seq.map string |> String.concat "; ") + "]"
        let bookTitles =
          strArray
            [
              for books in suggestion.Carts ->
                strArray [for b in books -> b.Title]
            ]
        printfn "%O %O" suggestion.ResultWallet bookTitles

      let displayDig (user: User) (myWallet: Money) (suggestion: Suggestion) =
        printfn "Dig: %O" suggestion.ResultWallet
        let mutable wallet = myWallet
        for (i, books) in suggestion.Carts |> Seq.indexed do
          printfn "  #%d account:" (i + 1)
          let payment = paymentOnCart user books
          printfn "    Payment = %O" payment
          printfn "    Wallet = %O" wallet
          wallet <- wallet |> Wallet.pay payment

      printfn "Theoretically best = %O" result.BestWallet

      printfn ""
      printfn "Recomended:"
      for suggestion in result.GoodSuggestions do
        displayFull suggestion

      if verbose then
        printfn ""
        printfn "Others:"
        for suggestion in result.BadSuggestions do
          displayCompact suggestion

        printfn ""
        printfn "Dig:"
        for suggestion in result.GoodSuggestions do
          printfn ""
          displayDig user userWallet suggestion

module App =
  open BookWalker
  open BookWalker.Logic

  let taxRate = 0.08

  let maxPurchaseCount = 3

  let book title price saleCoinRate =
    {
      Title = title
      Price = float price
      SaleCoinRate = saleCoinRate
    }

  let me: User =
    {
      TaxRate = 0.08
      CoinRate = 0.01
    }

  let myWallet =
    {
      Jpy = 0.0
      Coin = 0.0
    }

  let myCheckList =
    let b = book
    [|
      b "Book 1" 630 0.49
      b "Book 2" 590 0.49
      b "Book 3" 650 0.49
      b "Book 4" 570 0.49
      b "Book 5" 590 0.49
      b "Book 6" 630 0.49
      b "Book 7" 580 0.49
    |]

  [<EntryPoint>]
  let main _ =
    let context =
      {
        User = me
        UserWallet = myWallet
        MaxPurchaseCount = maxPurchaseCount
      }: BookWalker.EvaluationContext

    let verbose = false

    Logic.suggest context myCheckList
    |> Logic.display verbose

    0
