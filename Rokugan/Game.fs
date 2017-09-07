module Game

open GameTypes

open PlayerState
open GameState


let rec gotoNextPhase (gs:GameState) =
    let gs' = gs |> cleanPhaseFlags |> Triggers.cleanPhaseTriggers
    match gs.GamePhase with
    | Dynasty -> gs' |> Draw.gotoDrawPhase gotoNextPhase
    | Draw -> gs' |> Conflict.gotoConflictPhase 
    | Conflict -> gs' |> Fate.gotoFatePhase gotoNextPhase
    | Fate -> gs' |> Regroup.gotoRegroupPhase gotoNextPhase
    | Regroup -> gs' |> nextRound |> Dynasty.gotoDynastyPhase
    | _ -> gs'

let createStartingRings = 
  [ {Element = Fire; State = Unclaimed; Fate = 0}
    {Element = Element.Water; State = Unclaimed; Fate = 0}
    {Element = Air; State = Unclaimed; Fate = 0}
    {Element = Earth; State = Unclaimed; Fate = 0}
    {Element = Void; State = Unclaimed; Fate = 0}] 

let startGame playerConfig1 playerConfig2 = 
    let firstPlayer = Utils.chooseRandomPlayer ()
    { 
        Rings = createStartingRings
        Triggers = []
        TurnNumber = 1
        ActivePlayer = firstPlayer
        Actions = []
        GamePhase = Dynasty
        FirstPlayer = firstPlayer
        Player1State = initializePlayerState playerConfig1
        Player2State = initializePlayerState playerConfig2  }
    |> addSecondPlayer1Fate
    |> Triggers.addWinConditionsTriggers
    |> Dynasty.gotoDynastyPhase
    |> Triggers.addDynastyPassTrigger gotoNextPhase

let playAction n gs =
    if n > gs.Actions.Length then (gs:GameState)
    else
        gs |> gs.Actions.[n].Action |> Triggers.applyTriggers    
