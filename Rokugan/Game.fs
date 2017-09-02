module Game

open GameTypes

open PlayerState
open GameState

let gotoDrawPhase gs =
    { gs with 
        GamePhase = Draw
        ActivePlayer = gs.FirstPlayer} |> Draw.getDrawPhaseActions 

let gotoNextPhase gs =
    let gs' = gs |> cleanPhaseFlags |> Triggers.cleanPhaseTriggers
    match gs.GamePhase with
    | Dynasty -> gs' |> gotoDrawPhase
    | _ -> gs'

let gotoDynastyPhase gs = 
    gs
    |> Dynasty.revealAllDynastyCardsAtProvinces
    |> Dynasty.collectFateFromStronghold
    |> Triggers.addDynastyPassTrigger gotoNextPhase
    |> Dynasty.addDynastyPhaseActions   
   
let initializeGameState playerConfig1 playerConfig2 = 
    let firstPlayer = Utils.chooseRandomPlayer ()
    { 
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