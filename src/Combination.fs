module BigTwo.Combination

open Card

type Combination =
    | Single of Card
    | Pair of Card * Card
    | Triple of Card * Card * Card
    | Run of Card list  // A combination of at least three cards that are in a numerical sequence. 
    | PairSequences of Card list // A combination of at least 3 pairs
    | FourOfAKind of Card list



type Parser<'T> = Parser of (Card list -> Result<'T * Card list, string>)

let run (Parser parser) (input : Card list) = parser input


let pairParser = 
    let rec inner = function
        | [] -> Error "not found pair"
        | head :: tail ->
            let x = List.tryFind ((=) head) tail
            match x with 
            | Some x -> 
                let remaining = tail |> List.filter ((<>) head)
                Ok (Pair (head, x), remaining)
            | None -> inner tail
    Parser inner

















// type ErrorMessage = string

// type Parser<'T> = Parser of (Card list -> Result<'T * Card list, ErrorMessage>)




// let satisfy (card : Card) =
//     let inner cards =
//         match cards with
//         | [] -> Error "no more cards"
//         | h :: _ when h <> card -> Error (sprintf "Expecting %A but got %A" card h)
//         | h :: t -> Ok (Single h, t)
//     Parser inner


// let run (Parser parser) (input : Card list) = parser input

// let returnM x = 
//     let inner input = Ok (x, input)
//     Parser inner

// let bind f m =
//     let inner input = 
//         let r1 = run m input
//         match r1 with
//         | Error err -> Error err
//         | Ok (v1, remaining) ->
//             let m2 = f v1 
//             run m2 remaining
//     Parser inner

// let (>>=) m f = bind f m 

// let map f = bind (f >> returnM)

// let (<!>) = map

// let apply fm xm = fm >>= (fun f -> xm >>= (f >> returnM))

// let (<*>) = apply

// let lift2 f xm ym = returnM f <*> xm <*> ym

// let andThen m1 m2 = m1 >>= (fun r1 -> m2 >>= (fun r2 -> returnM (r1, r2)))

// let (.>>.) = andThen

// let orElse m1 m2 = 
//     let inner input =
//         let r1 = run m1 input
//         match r1 with
//         | Ok _ -> r1
//         | Error _ -> run m2 input
//     Parser inner

// let (<|>) = orElse

// let choice parsers = List.reduce orElse parsers

// let anyOf (cards : Card list) = 
//     cards 
//     |> List.map satisfy
//     |> choice

// let rec sequence parsers = 
//     let cons h t = h :: t
//     let consM = lift2 cons
//     match parsers with
//     | [] -> returnM []
//     | h :: t -> consM h (sequence t)

// let (>=>) f1 f2 x = f1 x >>= f2

// let (>>%) m v = m >>= (fun _ -> returnM v)

// let (>>.) m1 m2 = m1 >>= (fun _ -> m2)

// let (.>>) m1 m2 = m1 >>= (fun x -> m2 >>% x)

// let between m1 m2 m3 =
//     m1 >>. m2 .>> m3



