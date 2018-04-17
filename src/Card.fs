module BigTwo.Card

type Rank = | Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = | Hearts | Diamonds | Clubs | Spades 

type Card = Rank * Suit

type Deck = Card list

type Player = { Name : string; Hand : Card list }

type Game = { Deck : Deck; Players : Player list; CurrentPlayer : Player; DiscardPile : Card list }

let allSuits = [ Hearts; Diamonds; Clubs; Spades ]

let allRanks = [ Ace; Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King ] 


let newDeck () : Deck = 
    [ for suit in allSuits do
        for rank in allRanks do 
            yield (rank, suit) ]

let rng = System.Random ()

let shuffle (deck : Deck) : Deck = 
    deck |> List.sortBy (fun _ -> rng.Next())




