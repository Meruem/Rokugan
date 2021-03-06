module Draw

open GameTypes
open RokuganShared

open PlayerState
open GameState
open PlayerActions

let applyBids gs =
    let pl1Bid = match gs.Player1State.Bid with | Some x -> x | None -> 0
    let pl2Bid = match gs.Player2State.Bid with | Some x -> x | None -> 0
    [AddHonor (Player1, (pl2Bid - pl1Bid)) 
     AddHonor (Player2, (pl1Bid - pl2Bid))
     DrawConflictCard (Player1, pl1Bid)
     DrawConflictCard (Player2, pl2Bid) ]

let rec bidActions gs =
    let pl1Bid = gs.Player1State.Bid.IsSome
    let pl2Bid = gs.Player2State.Bid.IsSome
    if pl1Bid && pl2Bid then []
    else
        let next = if (not pl1Bid) && (not pl2Bid) then Some bidActions else None
        let l1 = if pl1Bid then [] else choicei Player1 "Player 1 bid" 1 5 (fun i -> change (Bid (Player1, i)) >+> playerActionsM next "Choose bid: ") 
        let l2 = if pl2Bid then [] else choicei Player2 "Player 2 bid" 1 5 (fun i -> change (Bid (Player2, i)) >+> playerActionsM next "Choose bid: ")
        l1 @ l2

let gotoDrawPhase nextPhase =
    changes
        [ChangePhase Draw
         CleanBids]
    >+> playerActions bidActions "Choose bid:"
    >+> act applyBids
    >+> Actions.actionWindow FirstPlayer "Draw phase action window: "
    >+!> nextPhase

let onApplyBids gs = (gs, applyBids gs)