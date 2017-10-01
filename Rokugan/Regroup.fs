module Regroup

open RokuganShared
open GameTypes
open GameState
open PlayerState
open Actions 

let readyAllCards (gs:GameState) = 
    gs.Cards 
    |> List.filter Card.isBowed 
    |> List.map Ready
    
let addFateToRings (gs:GameState) = 
    gs.Rings 
    |> List.filter Ring.isUnclaimed
    |> List.map (fun ring -> AddFateOnRing (ring,1))

let returnRings (gs:GameState) =
    gs.Rings 
    |> List.filter Ring.isUnclaimed 
    |> List.map ReturnRing

let gotoRegroupPhase gotoNextPhase (gs:GameState) =
    let drawAndDiscardCards cards = 
        let drawAndDiscardCard card =
            match card.Zone with 
            | DynastyInProvinces pos -> [DrawDynastyCard (card.Owner,pos)]
            | _ -> []
            @ [DiscardFromPlay card] 
        changes (cards |> List.collect drawAndDiscardCard)
        
    changes
        ([ChangePhase Regroup]
        @ (readyAllCards gs)
        @ (addFateToRings gs)
        @ (returnRings gs)
        @ [NextRound])
    >+> chooseDynastyInProvince drawAndDiscardCards
    >+!> gotoNextPhase

