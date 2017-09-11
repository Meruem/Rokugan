module Game

open GameTypes

open PlayerState
open GameState


let rec gotoNextPhase (gs:GameState) =
    let gs' = gs |> cleanPhaseFlags |> Triggers.cleanPhaseTriggers |> Conflict.cleanDeclaredConflicts
    match gs.GamePhase with
    | Dynasty -> gs' |> Draw.gotoDrawPhase gotoNextPhase
    | Draw -> gs' |> Conflict.gotoConflictPhase 
    | Conflict -> gs' |> Fate.gotoFatePhase gotoNextPhase
    | Fate -> gs' |> Regroup.gotoRegroupPhase gotoNextPhase
    | Regroup -> gs' |> nextRound |> Dynasty.gotoDynastyPhase
    | _ -> gs'

let createStartingRings = 
  [ Ring.createRing Element.Fire
    Ring.createRing Element.Water
    Ring.createRing Element.Air
    Ring.createRing Element.Earth
    Ring.createRing Element.Void ]

let startGame playerConfig1 playerConfig2 firstPlayer = 
    { 
        Rings = createStartingRings
        Triggers = []
        TurnNumber = 1
        ActivePlayer = firstPlayer
        Actions = []
        GamePhase = Dynasty
        FirstPlayer = firstPlayer
        Player1State = initializePlayerState playerConfig1 Player1
        Player2State = initializePlayerState playerConfig2 Player2 }
    |> addSecondPlayer1Fate
    |> Triggers.addWinConditionsTriggers
    |> Triggers.addDynastyPassTrigger gotoNextPhase
    |> Triggers.addConflictEndTrigger gotoNextPhase
    |> Dynasty.gotoDynastyPhase

let playAction n gs =
    if n > gs.Actions.Length then (gs:GameState)
    else
        gs |> gs.Actions.[n].Action |> Triggers.applyTriggers    
