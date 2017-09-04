module Game

open GameTypes

open PlayerState
open GameState

let gotoDrawPhase gotoNextPhase gs =
    { gs with 
        GamePhase = Draw
        ActivePlayer = gs.FirstPlayer} |> Draw.getDrawPhaseActions gotoNextPhase 
let gotoConflictPhase gs = 
    { gs with
        GamePhase = Conflict
        ActivePlayer = gs.FirstPlayer }
    |> Conflict.getConflictPhaseActions

let gotoDynastyPhase gotoNextPhase gs = 
    gs
    |> Dynasty.revealAllDynastyCardsAtProvinces
    |> Dynasty.collectFateFromStronghold
    |> Triggers.addDynastyPassTrigger gotoNextPhase
    |> Dynasty.addDynastyPhaseActions   

let rec gotoNextPhase gs =
    let gs' = gs |> cleanPhaseFlags |> Triggers.cleanPhaseTriggers
    match gs.GamePhase with
    | Dynasty -> gs' |> gotoDrawPhase gotoNextPhase
    | Draw -> gs' |> gotoConflictPhase 
    | _ -> gs'

let startGame = gotoDynastyPhase gotoNextPhase

let createStartingRings = 
  [ {Element = Fire; State = Unclaimed; Fate = 0}
    {Element = Element.Water; State = Unclaimed; Fate = 0}
    {Element = Air; State = Unclaimed; Fate = 0}
    {Element = Earth; State = Unclaimed; Fate = 0}
    {Element = Void; State = Unclaimed; Fate = 0}] 

let initializeGameState playerConfig1 playerConfig2 = 
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

let playAction n gs =
    if n > gs.Actions.Length then gs
    else
        gs |> gs.Actions.[n].Action |> Triggers.applyTriggers    