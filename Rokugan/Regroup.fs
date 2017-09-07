module Regroup

open GameTypes
open GameState

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
    let chooseDynastyToDiscard gs =
        Actions.
    gs 
    |> readyAllCards
    |> addFateToRings
    |> returnRings

