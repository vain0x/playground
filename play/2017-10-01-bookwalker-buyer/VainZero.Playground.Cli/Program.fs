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

module Program =
  let (-->) x y = (x, y)

  let negate x = -x

  // 長さ n の配列を最大 r 箇所で区切り、いくつかの空でない部分に分割する。
  let separate n r =
    let rec loop i r separators =
      seq {
        if i = n then
          yield separators
        else
          if r >= 1 && i >= 1 then
            yield! loop (i + 1) (r - 1) (i :: separators)
          yield! loop (i + 1) r separators
      }
    loop 0 r []

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

    [<AutoOpen>]
    module Money =
      let jpy (money: Money) =
        money.Jpy

      let coin (money: Money) =
        money.Coin

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

    [<AutoOpen>]
    module Logic =
      let paymentOnPurchase (user: User) (checkedBook: CheckedBook) =
        let price = float checkedBook.Price
        let ephemeralCoin = price * checkedBook.SaleCoinRate
        let durableCoin = price * user.CoinRate
        {
          Jpy = -price * (user.TaxRate + 1.0)
          Coin = ephemeralCoin + durableCoin
        }

      let paymentOnPurchaseMany user checkedBooks =
        checkedBooks
        |> Seq.map (fun b -> paymentOnPurchase user b)
        |> Money.sum

      let coinAmountOnPurchase user checkedBook =
        paymentOnPurchase user checkedBook
        |> Money.coin

      // 会計後の財布を計算する。
      let pay (payment: Money) (wallet: Money) =
        let total =
          payment |> Money.jpy |> negate
        let consumedCoin =
          wallet |> Money.coin |> min total
        {
          Jpy =
            wallet.Jpy - (total - consumedCoin)
          Coin =
            wallet.Coin - consumedCoin + payment.Coin
        }

      // 複数回の会計を行った後の財布を計算する。
      let payMany payments wallet =
        payments |> Seq.fold (fun wallet payment -> pay payment wallet) wallet

      let goodSuggestions suggestions =
        suggestions
        |> Seq.groupBy (fun (_, _, bss) -> bss |> Array.length)
        |> Seq.map (fun (_, g) -> g |> Seq.minBy (fun (_, w, _) -> w |> Money.coin))
        |> Seq.sortBy (fun (_, w, _) -> w |> Money.coin)
        |> Seq.toArray

      // コインの残量を最小化する会計方法を計算する。
      // 考察:
      // 最終的に生じる請求額と還元されるコイン数は変化しないが、セール中に取得したコインをセール中に使用することで残量を減らせる。つまり、会計時に使用するコインの枚数を最大化すればよい。商品は、コイン還元量が多いものを先に買うほうが常に有利。また、会計が細かいほどコインを使用する機会が多くなる。したがって、コイン還元量の多い順に1つずつ会計に通していくことで最善の結果が得られる。
      // 現実的には数回の会計で済ませたい。1回の会計で複数の商品を購入するように変更して、会計回数を最小化する。いま請求額と還元コインのみに注目しているので、1回の会計で複数の商品を購入するのは、1つの大きな商品を購入するのと同じ。先ほどの考察から、購入する商品群を数個の部分集合に分割して、コイン還元量の多い順に会計に通すのが最適。
      // (なお、会計をまとめることで端数処理が変化し、若干の損になるが、金額が小さいので無視する。)
      // 解法:
      // - 会計回数は最大3回とする。
      // - 商品をコイン還元量の多い順に並べて、これを列 bs とする。
      // - bs に2つの「区切り」を入れて3つの連続部分列に分割する。区切りの入れ方は全探索 O(n^2) で問題ない。ただし2つ目の区切りは2分探索で求まるので、O(n log n) にもできる。
      let suggest (user: User) (wallet: Money) (checkedBooks: CheckedBook[]) =
        let checkedBooks =
          checkedBooks |> Array.sortByDescending (fun b -> coinAmountOnPurchase user b)
        let bestWallet =
          checkedBooks
          |> Seq.map (fun b -> paymentOnPurchase user b)
          |> fun payments -> payMany payments wallet
        let suggestions =
          seq {
            let n = checkedBooks |> Array.length
            for r in 0..(min 2 (n - 1)) do
              for separators in separate n r do
                let bookListList =
                  [|
                    for (l, r) in (0 :: (n :: separators |> List.rev)) |> List.pairwise ->
                      ArraySegment(checkedBooks, l, r - l) |> Seq.toArray
                  |]
                let wallet =
                  bookListList
                  |> Seq.map (fun books -> paymentOnPurchaseMany user books)
                  |> fun payments -> payMany payments wallet
                yield (wallet, bookListList)
          }
          |> Seq.mapi (fun i (w, bss) -> (i, w, bss))
          |> Seq.toArray
        (bestWallet, suggestions)

      let displaySuggestions (bestWallet, suggestions: (int * Money * _)[]) =
        let goods =
          suggestions
          |> goodSuggestions
        let others =
          let goodIndexes = goods |> Seq.map (fun (i, _, _) -> i) |> Set.ofSeq
          suggestions
          |> Seq.filter (fun (i, _, _) -> goodIndexes.Contains(i) |> not)
          |> Seq.sortBy (fun (_, w, _) -> w.Coin)
          |> Seq.toArray

        printfn "Best: %O" bestWallet
        printfn ""

        printfn "Recomended:"
        for (_, wallet, bookListList) in goods do
          printfn "- %d %O" (bookListList |> Array.length) wallet
          for (i, books) in bookListList |> Seq.indexed do
            printfn "  - %d:" (i + 1)
            for book in books do
              printfn "    - %s" book.Title
        printfn ""

        printfn "Others:"
        for (_, wallet, bookListList) in others do
          let s xs = "[" + (xs |> Seq.map string |> String.concat "; ") + "]"
          let bookNames = s [for books in bookListList -> s [for b in books -> b.Title]]
          printfn "%O %O" wallet bookNames

  open BookWalker

  let taxRate = 0.08

  let saleCoinRate = 0.49

  let book title price =
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
      Coin = 946.0
    }

  let myCheckList =
    let b = book
    [|
      b "Book 1" 630
      b "Book 2" 590
      b "Book 3" 650
      b "Book 4" 570
      b "Book 5" 590
      b "Book 6" 630
      b "Book 7" 580
    |]

  [<EntryPoint>]
  let main _ =
    let user = me
    let wallet = myWallet
    let checkedBooks = myCheckList

    let (bestWallet, suggestions) = suggest user wallet checkedBooks
    displaySuggestions (bestWallet, suggestions)

    for (_, resultWallet, bookss) in suggestions |> goodSuggestions do
      printfn ""
      printfn "Dig: %O" resultWallet
      let mutable wallet = myWallet
      for (i, books) in bookss |> Seq.indexed do
        printfn "  #%d account:" (i + 1)
        let payment = paymentOnPurchaseMany user books
        printfn "    Payment = %O" payment
        printfn "    Wallet = %O" wallet
        wallet <- pay payment wallet
    0
