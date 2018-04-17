module BigTwo.Card

type Rank = | Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = | Hearts | Diamonds | Clubs | Spades 

type Card = {
    Rank : Rank
    Suit : Suit
}

type Deck = Card list

type Player = { Name : string; Hand : Card list }

type Game = { Deck : Deck; Players : Player list; CurrentPlayer : Player; DiscardPile : Card list }


let newDeck () : Deck = 
    let allSuits = [ Hearts; Diamonds; Clubs; Spades ]
    let allRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ] 
    [ for suit in allSuits do
        for rank in allRanks do 
            yield { Suit = suit; Rank = rank } ]

let rng = System.Random ()

let shuffle (deck : Deck) : Deck = 
    deck |> List.sortBy (fun _ -> rng.Next())




