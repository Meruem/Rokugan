module Draw

open GameTypes
open PlayerState
open GameState

let applyBids pl1Bid pl2Bid gotoNextPhase gs =
    { gs with
        Actions = []
        Player1State = gs.Player1State |> changeBid pl1Bid |> addHonor (pl2Bid - pl1Bid) |> drawConflictCards pl1Bid
        Player2State = gs.Player2State |> changeBid pl2Bid |> addHonor (pl1Bid - pl2Bid) |> drawConflictCards pl2Bid}
    |> gotoNextPhase

let getDrawPhaseActions gotoNextPhase gs =
    let nextActionPl1 pl1Bid gs = { gs with Actions = createChoiceActions (fun i -> applyBids pl1Bid i gotoNextPhase) "Player 2 bid" 1 5 } 
    let nextActionPl2 pl2Bid gs = { gs with Actions = createChoiceActions (fun i -> applyBids i pl2Bid gotoNextPhase) "Player 1 bid" 1 5 } 
    { gs with 
        Actions = List.append (createChoiceActions nextActionPl1 "Player 1 bid" 1 5) (createChoiceActions nextActionPl2 "Player 2 bid" 1 5) }