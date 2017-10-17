module Triggers

open RokuganShared
open GameTypes
open CardDef
let addTrigger (trigger: GameTrigger<'a,'b,'c>) (gm:GameModel<'a,'b,'c>) =
   {gm with Triggers = trigger :: gm.Triggers} 

let removeTrigger id (gm:GameModel<'a,'b,'c>) =
    { gm with Triggers = gm.Triggers |> List.filter (fun t -> t.Id <> id)}  

// let applyTriggers gs = 
//     let triggers = gs.Triggers |> List.filter (fun t -> t.Condition gs)
//     let removeOnlyOnceTrigger gs = if triggers.[0].Lifetime = Once then gs |> removeTrigger triggers.[0].Name else gs
//     if triggers.Length = 0 then gs
//     else 
//         gs |> removeOnlyOnceTrigger |> triggers.[0].Action // not sure what to do with multiple triggers yet :(    

// let cleanPhaseTriggers gs = 
//     { gs with Triggers = gs.Triggers |> List.filter (fun t -> t.Lifetime <> Phase)}    

let addWinConditionsTriggers (gs:GameModel<GameState,Command<GameState,PlayerActionType>,'c>) =
    let pl1NoHonor cmd gs = gs.Player1State.Honor <= 0
    let pl2NoHonor cmd gs = gs.Player2State.Honor <= 0
    let pl1over25honor cmd gs = gs.Player1State.Honor >= 25
    let pl2over25honor cmd gs = gs.Player2State.Honor >= 25
    let pl1BrokenStronghold cmd gs = gs.Player1State.StrongholdProvince |> Card.isProvinceBroken    
    let pl2BrokenStronghold cmd gs = gs.Player2State.StrongholdProvince |> Card.isProvinceBroken  
    let pl2Win _ = changes [EndGame Player2Won] // {gs with GamePhase = End Player2Won; Actions = [] }
    let pl1Win _ = changes [EndGame Player1Won] // {gs with GamePhase = End Player1Won; Actions = [] }
    let triggers = [
      { Id = Utils.newId ()
        GameTrigger.Name = "Player1 military victory"
        Lifetime = Once
        Condition = pl2BrokenStronghold
        Effect = pl1Win }
      { Id = Utils.newId ()
        Name = "Player2 military victory"
        Lifetime = Once
        Condition = pl1BrokenStronghold
        Effect = pl2Win }
      { Id = Utils.newId ()
        Name = "Player1 no honor defeat"
        Lifetime = Once
        Condition = pl1NoHonor
        Effect = pl2Win }
      { Id = Utils.newId ()
        Name = "Player2 no honor defeat"
        Lifetime = Once
        Condition = pl2NoHonor
        Effect = pl1Win }
      { Id = Utils.newId ()
        Name = "Player1 honor victory"
        Lifetime = Once
        Condition = pl1over25honor
        Effect = pl1Win }
      { Id = Utils.newId () 
        Name = "Player2 honor victory"
        Lifetime = Once
        Condition = pl2over25honor
        Effect = pl2Win }]
    { gs with Triggers = List.append triggers gs.Triggers}

let gameTrigger name lifetime condition transform =
  { Id = Utils.newId ()
    GameTrigger.Name = name
    Lifetime = lifetime 
    Condition = condition
    Effect = transform }

let fromCardTrigger (cardId:CardId) ct =
    gameTrigger 
        (sprintf "[%d]: %s" cardId ct.Name)
        ct.Lifetime
        (ct.Condition cardId)
        (ct.Effect cardId)


let addCardTriggers (card:Card) (gm:GameModel<GameState, Command<GameState,PlayerActionType>, PlayerActionType>) = 
    let cardDef = CardRepository.repository.GetCard card.Title
    let newTriggers =
        cardDef.Triggers 
        |> List.map (fromCardTrigger card.Id)
    {gm with Triggers = newTriggers @ gm.Triggers}

let addAllCardsTriggers (gm:GameModel<GameState, Command<GameState,PlayerActionType>, PlayerActionType>) = 
    gm.State.Cards 
    |> List.fold (fun acc card -> addCardTriggers card acc) gm

