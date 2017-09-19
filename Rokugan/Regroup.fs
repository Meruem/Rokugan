module Regroup

open GameTypes
open GameState
open PlayerState

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
        (cards |> List.collect drawAndDiscardCard)
        @+ gotoNextPhase gs
        
    let cmds =
        [ChangePhase Regroup]
        @ (readyAllCards gs)
        @ (addFateToRings gs)
        @ (returnRings gs)
        @ [NextRound]
    let actions = Actions.chooseDynastyInProvince (drawAndDiscardCards)
    transform cmds actions.NextActions

