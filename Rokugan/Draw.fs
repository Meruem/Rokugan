module Draw

open GameTypes
open PlayerState
open GameState
open Actions

let applyBids pl1Bid pl2Bid gotoNextPhase gs =
    { gs with
        Actions = []
        Player1State = 
            gs.Player1State 
            |> changeBid pl1Bid 
            |> addHonor (pl2Bid - pl1Bid) 
            |> drawConflictCards pl1Bid
        Player2State = 
            gs.Player2State 
            |> changeBid pl2Bid 
            |> addHonor (pl1Bid - pl2Bid) 
            |> drawConflictCards pl2Bid}
    |> gotoNextPhase

let getDrawPhaseActions gotoNextPhase gs =
    let nextActionPl1 pl1Bid gs = 
        gs >!=> choicei  "Player 2 bid" 1 5 (fun i -> applyBids pl1Bid i gotoNextPhase) 
    let nextActionPl2 pl2Bid gs = 
        gs >!=> choicei  "Player 1 bid" 1 5 (fun i -> applyBids i pl2Bid gotoNextPhase)
    gs 
        >!=> choicei "Player 1 bid" 1 5 nextActionPl1
        >+=> choicei "Player 2 bid" 1 5 nextActionPl2 

let gotoDrawPhase gotoNextPhase (gs:GameState) =
    { gs with 
        GamePhase = Draw
        ActivePlayer = gs.FirstPlayer} |> getDrawPhaseActions gotoNextPhase 