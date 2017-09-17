module Draw

open GameTypes
open PlayerState
open GameState
open Actions

let applyBids gs =
    let pl1Bid = match gs.Player1State.Bid with | Some x -> x | None -> 0
    let pl2Bid = match gs.Player2State.Bid with | Some x -> x | None -> 0
    [AddHonor (Player1, (pl2Bid - pl1Bid)) 
     AddHonor (Player2, (pl1Bid - pl2Bid))
     DrawConflictCard (Player1, pl1Bid)
     DrawConflictCard (Player2, pl2Bid) ]

let rec drawPhaseActions (continuation:GameState -> Transform) gs =
    let pl1NoBid = gs.Player1State.Bid = None
    let pl2NoBid = gs.Player2State.Bid = None
    let noBid = pl1NoBid && pl2NoBid 
    let nextActions = 
        if noBid then drawPhaseActions continuation
        else (continuation gs).NextActions 
    let nextCommands = if noBid then [] else ApplyBids :: (continuation gs).Commands
    let l1 = if pl1NoBid then choicei Player1 "Player 1 bid" 1 5 (fun i -> {Commands = [Bid (Player1, i)] @ nextCommands; NextActions = nextActions}) else []
    let l2 = if pl2NoBid then choicei Player2 "Player 2 bid" 1 5 (fun i -> {Commands = [Bid (Player2, i)] @ nextCommands; NextActions = nextActions}) else []
    l1 @ l2

let gotoDrawPhase nextPhase gs =
    { Commands =
        [ChangePhase Draw
         CleanBids]
      NextActions = drawPhaseActions nextPhase}

let onApplyBids gs = (gs, applyBids gs)