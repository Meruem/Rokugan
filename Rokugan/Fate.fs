module Fate

open GameTypes
open GameState

let remove1FateorDiscard (card:Card) = 
    if card.Fate > 0 then (AddFateOnCard (card, -1))
    else (DiscardFromPlay card)

let gotoFatePhase gotoNextPhase (gs:GameState) =
    changes 
        ([ChangePhase Fate]
        @ (gs.Cards |> List.filter (fun c -> Card.isCharacter c && c.Zone = Home) |> List.map remove1FateorDiscard))
    >+!> gotoNextPhase