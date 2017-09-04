module GameState
open GameTypes
open System
open PlayerState

let changePlayerState player playerStateChange gs =
    match player with
    | Player1 -> {gs with Player1State = gs.Player1State |> playerStateChange}
    | Player2 -> {gs with Player2State = gs.Player2State |> playerStateChange}

let changeActivePlayerState playerStateChange gs = changePlayerState gs.ActivePlayer playerStateChange gs
let changeOtherPlayerState playerStateChange gs = changePlayerState (otherPlayer gs.ActivePlayer) playerStateChange gs

let addSecondPlayer1Fate gs =
    let otherPl = otherPlayer gs.FirstPlayer
    let add1Fate (ps:PlayerState) = {ps with Fate = ps.Fate + 1}
    gs |> changePlayerState otherPl add1Fate

let hasPlayerPassed player gs =
    match player with
    | Player1 -> hasPassed gs.Player1State
    | Player2 -> hasPassed gs.Player2State

let switchActivePlayer gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPlayerPassed otherPl gs then gs
    else { gs with ActivePlayer = otherPl}

let playerState player gs =
    match player with
    | Player1 -> gs.Player1State
    | Player2 -> gs.Player2State

let activePlayerState gs = playerState gs.ActivePlayer gs

let otherPlayerState gs = playerState (otherPlayer gs.ActivePlayer) gs

let passActive = changeActivePlayerState pass

let createChoiceActions nextAction desc min max =
    [min..max]
        |> List.map (fun i -> 
            { Action = nextAction i 
              Type = Choice (i, desc) })

let addChoiceActions actionList gs =
    {gs with Actions = actionList}

let cleanPhaseFlags gs =
    { gs with 
        Player1State = cleanPhaseFlags gs.Player1State
        Player2State = cleanPhaseFlags gs.Player2State }

let removeFateFromRing ring gs =
    { gs with Rings = gs.Rings |> Utils.replaceListElement { ring with Fate = 0 } (fun r -> r.Element = ring.Element) }