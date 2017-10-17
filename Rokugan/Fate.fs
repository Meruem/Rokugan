module Fate

open RokuganShared
open GameTypes
open GameState

let remove1FateorDiscard (card:Card) = 
    if card.Fate > 0 then (AddFateOnCard (card.Id, -1))
    else (DiscardFromPlay card.Id)

let checkAllCardsForDiscard (gs:GameState) =
    gs.Cards 
    |> List.filter (fun c -> Card.isCharacter c && c.Zone = Home) 
    |> List.map remove1FateorDiscard

let addFateToRings (gs:GameState) = 
    gs.Rings 
    |> List.filter Ring.isUnclaimed
    |> List.map (fun ring -> AddFateOnRing (ring,1))

let gotoFatePhase gotoNextPhase =
    changes [ChangePhase Fate]
    >+> act checkAllCardsForDiscard
    >+> act addFateToRings
    >+> Actions.actionWindow FirstPlayer "Fate phase action window: "
    >+!> gotoNextPhase