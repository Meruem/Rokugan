module Command

open RokuganShared
open GameTypes

let removeCardState state (card:Card) =
    if not (card.States.Contains state) then None
    else Some (RemoveCardState (state, card))
