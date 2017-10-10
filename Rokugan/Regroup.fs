module Regroup

open RokuganShared
open GameTypes
open GameState
open PlayerState
open PlayerActions 

let readyAllCards (gs:GameState) = 
    gs.Cards 
    |> List.filter Card.isBowed 
    |> List.map Ready
    
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
            @ [DiscardFromPlay card] 
        changes (cards |> List.collect drawAndDiscardCard)
        
    changes [ChangePhase Regroup]
    >+> Actions.actionWindow FirstPlayer
    >+> act readyAllCards
    >+> chooseDynastyInProvince drawAndDiscardCards
    >+> act returnRings
    >+> changes [NextRound]
    >+!> gotoNextPhase

