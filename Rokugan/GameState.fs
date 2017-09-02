module GameState
open GameTypes
open System
open PlayerState

let changePlayerState player playerStateChange gs =
    match player with
    | Player1 -> {gs with Player1State = gs.Player1State |> playerStateChange}
    | Player2 -> {gs with Player2State = gs.Player2State |> playerStateChange}

let changeActivePlayerState playerStateChange gs = changePlayerState gs.ActivePlayer playerStateChange gs

let addSecondPlayer1Fate gs =
    let otherPl = otherPlayer gs.FirstPlayer
    let add1Fate (ps:PlayerState) = {ps with Fate = ps.Fate + 1}
    gs |> changePlayerState otherPl add1Fate

let getPlayerState player gs = match player with | Player1 -> gs.Player1State | Player2 -> gs.Player2State

let hasPlayerPassed player gs =
    match player with
    | Player1 -> hasPassed gs.Player1State
    | Player2 -> hasPassed gs.Player2State

let switchActivePlayer gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPlayerPassed otherPl gs then gs
    else { gs with ActivePlayer = otherPl}

let activePlayerState gs =
    match gs.ActivePlayer with
    | Player1 -> gs.Player1State
    | Player2 -> gs.Player2State

let passActive = changeActivePlayerState pass

let createChoiceActions nextAction min max =
    [min..max]
        |> List.map (fun i -> 
            { Action = nextAction i 
              Type = Choice i })

let addChoiceActions actionList gs =
    {gs with Actions = actionList}

let cleanPhaseFlags gs =
    { gs with 
        Player1State = cleanPhaseFlags gs.Player1State
        Player2State = cleanPhaseFlags gs.Player2State }