module Regroup

open RokuganShared
open GameTypes
open GameState
open PlayerState
open PlayerActions 

let readyAllCards (gs:GameState) = 
    gs.Cards 
    |> List.filter Card.isBowed 
    |> List.map (fun c -> Ready c.Id)
    
let returnRings (gs:GameState) =
    gs.Rings 
    |> List.filter Ring.isUnclaimed 
    |> List.map ReturnRing

let gotoRegroupPhase gotoNextPhase =
    let drawAndDiscardCards cards = 
        let drawAndDiscardCard card =
            match card.Zone with 
            | DynastyInProvinces pos -> [DrawDynastyCard (card.Owner,pos)]
            | _ -> []
            @ [DiscardFromPlay card.Id] 
        changes (cards |> List.collect drawAndDiscardCard)
        
    changes [ChangePhase Regroup]
    >+> Actions.actionWindow FirstPlayer "Regroup phase action window: "
    >+> act readyAllCards
    >+> chooseDynastyInProvince drawAndDiscardCards
    >+> act returnRings
    >+> changes [NextRound]
    >+!> gotoNextPhase

