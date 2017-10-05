module Command

open RokuganShared
open GameTypes

let removeCardState state (card:Card) =
    if (card.States.Contains state) then Some (RemoveCardState (state, card))
    else None
