module Regroup

open GameTypes
open GameState
open PlayerState

let readyAllCards gs = gs |> changeCards Card.ready gs.Cards
let addFateToRings gs = 
    {gs with 
        Rings = 
            gs.Rings 
            |> List.map (fun r -> if r.State = Unclaimed then {r with Fate = r.Fate + 1} else r)}

let returnRings gs =
    {gs with
        Rings = gs.Rings |> List.map (fun r -> {r with State = Unclaimed})}

let gotoRegroupPhase gotoNextPhase gs =
    let drawAndDiscardCards cards gs = 
        let drawAndDiscardCard card (gs:GameState) =
            match card.Zone with 
            | DynastyInProvinces pos -> drawDynasty pos card.Owner gs
            | _ -> gs
            |> changeCard Card.discard card
        cards |> List.fold (fun acc c -> drawAndDiscardCard c acc) gs
        |> gotoNextPhase
    let gs' =
        gs 
        |> readyAllCards
        |> addFateToRings
        |> returnRings
    gs' >!=> (Actions.chooseDynastyInProvince (drawAndDiscardCards) gs')

