module Triggers

open GameTypes

let addWinConditionsTriggers gs =
    let pl1NoHonor gs = gs.Player1State.Honor <= 0
    let pl2NoHonor gs = gs.Player2State.Honor <= 0
    let pl1over25honor gs = gs.Player1State.Honor >= 25
    let pl2over25honor gs = gs.Player2State.Honor >= 25
    let pl1BrokenStronghold gs = gs.Player1State.StrongholdProvince.State = Broken    
    let pl2BrokenStronghold gs = gs.Player2State.StrongholdProvince.State = Broken 
    let pl2Win gs = {gs with GamePhase = End Player2Won; Actions = [] }
    let pl1Win gs = {gs with GamePhase = End Player1Won; Actions = [] }
    let triggers = [
      { Name = "Player1 military victory"
        Lifetime = Game
        Condition = pl2BrokenStronghold
        Action = pl1Win }
      { Name = "Player2 military victory"
        Lifetime = Game
        Condition = pl1BrokenStronghold
        Action = pl2Win }
      { Name = "Player1 no honor defeat"
        Lifetime = Game
        Condition = pl1NoHonor
        Action = pl2Win }
      { Name = "Player2 no honor defeat"
        Lifetime = Game
        Condition = pl2NoHonor
        Action = pl1Win }
      { Name = "Player1 honor victory"
        Lifetime = Game
        Condition = pl1over25honor
        Action = pl1Win }
      { Name = "Player2 honor victory"
        Lifetime = Game
        Condition = pl2over25honor
        Action = pl2Win }]
    { gs with Triggers = List.append triggers gs.Triggers}

let addDynastyPassTrigger gotoNextPhase gs =
    let pl1Pass gs = gs.Player1State |> PlayerState.hasPlayerFlag Passed
    let pl2Pass gs = gs.Player2State |> PlayerState.hasPlayerFlag Passed
    let bothPlPass gs = pl1Pass gs && pl2Pass gs
    let trigger = 
      { Name = "Pass trigger"
        Lifetime = Phase
        Condition = bothPlPass
        Action =  gotoNextPhase }
    {gs with Triggers = trigger :: gs.Triggers}    

let applyTriggers gs = 
    let triggers = gs.Triggers |> List.filter (fun t -> t.Condition gs)
    if triggers.Length = 0 then gs
    else triggers.[0].Action gs // not sure what to do with multiple triggers yet :(    

let cleanPhaseTriggers gs = 
    { gs with Triggers = gs.Triggers |> List.filter (fun t -> t.Lifetime <> Phase)}        