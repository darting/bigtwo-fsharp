module Tests

open System
open Xunit
open BigTwo.Card
open BigTwo.Combination

[<Fact>]
let ``My test`` () =
    Assert.True(true)



[<Fact>]
let ``Test pair parser`` () =
    let cards = [ Ace, Hearts; Ace, Spades; Two, Hearts ] 

    let pair = run pairParser cards

    match pair with
    | Ok (comb, _) -> 
        match comb with
        | Pair (x, y) when x = y -> ()
        | x -> 
            Assert.True(false, sprintf "%A" x)
    | Error x -> 
        Assert.True(false, sprintf "%A" x)
