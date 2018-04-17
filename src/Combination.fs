module BigTwo.Combination

open Card

type Combination =
    | Single of Card
    | Pair of Card * Card
    | Triple of Card * Card * Card
    | Run of Card list

