module Fate

open RokuganShared
open GameTypes
open GameState

let remove1FateorDiscard (card:Card) = 
    if card.Fate > 0 then (AddFateOnCard (card, -1))
    else (DiscardFromPlay card)

let checkAllCardsForDiscard (gs:GameState) =
    gs.Cards 
    |> List.filter (fun c -> Card.isCharacter c && c.Zone = Home) 
    |> List.map remove1FateorDiscard

let gotoFatePhase gotoNextPhase =
    changes [ChangePhase Fate]
    >+> act checkAllCardsForDiscard
    >+!> gotoNextPhase