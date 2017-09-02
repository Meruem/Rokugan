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


let revealAllDynastyCardsAtProvinces gs =
    let removeHiddenState = Card.removeCardState Hidden
    let removeHiddenFromPlayerState pState =
        { pState with DynastyInProvinces = pState.DynastyInProvinces.Cards |> List.map removeHiddenState |> Zone }
    { gs with 
        Player1State = removeHiddenFromPlayerState gs.Player1State
        Player2State = removeHiddenFromPlayerState gs.Player2State }

let getPlayerState player gs = match player with | Player1 -> gs.Player1State | Player2 -> gs.Player2State

let playDynastyCard position addFate gs =
    let changeState state =
        let card = state.DynastyInProvinces.Cards.[position]
        let card' = { card with Fate = addFate}
        let home = card' :: state.Home.Cards
        let newCard, state' = drawCardFromDynastyDeck state
        let cardDef = CardRepository.getCharacterCard card.Title
        // add new dynasty card
        let dynastyInProvinces' = 
            state.DynastyInProvinces.Cards 
            |> Utils.replaceListElement newCard position
        {state' with 
            Home = Zone home
            DynastyInProvinces = Zone dynastyInProvinces'
            Fate = state'.Fate - cardDef.Cost - addFate}
    gs |> changeActivePlayerState changeState    

let collectFateFromStronghold gs =
    let stronghold (pState:PlayerState) = pState.Stonghold.StrongholdCard.Title |> CardRepository.getCard
    let fate pState =
        match (stronghold pState).Spec with 
        | Stronghold s -> s.FatePerRound
        | _ -> failwith "Stronghold card is not stronghold card"

    let p1Add = fate gs.Player1State
    let p2Add = fate gs.Player2State
    { gs with 
        Player1State = (addFateToPlayer p1Add gs.Player1State)    
        Player2State = (addFateToPlayer p2Add gs.Player2State)  }

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

let add1fateIfPassedFirst gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPassed (getPlayerState otherPl gs) then gs
    else 
        let add1Fate (state:PlayerState) = {state with Fate = state.Fate + 1} 
        gs |> changeActivePlayerState add1Fate

let rec getDynastyPhaseActions gs =
    let playerState = activePlayerState gs
    let actions = 
        getPlayableDynastyPositions playerState
        |> List.map (fun (pos, remainingFate) ->
            let nextAction fate = playDynastyCard pos fate >> switchActivePlayer >> getDynastyPhaseActions
            { 
                Action = createChoiceActions nextAction 0 remainingFate |> addChoiceActions
                Type = PlayCharacter playerState.DynastyInProvinces.Cards.[pos].Title })
    if hasPassed playerState then { gs with Actions = actions}
    else 
        let passAction = 
            { Type = PlayerActionType.Pass 
              Action = passActive >> add1fateIfPassedFirst >> switchActivePlayer >> getDynastyPhaseActions } 
        let actions' = passAction :: actions
        { gs with Actions = actions'}

let applyTriggers gs = 
    let triggers = gs.Triggers |> List.filter (fun t -> t.Condition gs)
    if triggers.Length = 0 then gs
    else triggers.[0].Action gs // not sure what to do with multiple triggers yet :(

let playAction n gs =
    if n > gs.Actions.Length then gs
    else
        gs |> gs.Actions.[n].Action |> applyTriggers

let cleanPhaseFlags gs =
    { gs with 
        Player1State = cleanPhaseFlags gs.Player1State
        Player2State = cleanPhaseFlags gs.Player2State }

let cleanPhaseTriggers gs = 
    { gs with Triggers = gs.Triggers |> List.filter (fun t -> t.Lifetime <> Phase)}

let getDrawPhaseActions gs =
    { gs with Actions = [] }

let gotoDrawPhase gs =
    { gs with 
        GamePhase = Draw
        ActivePlayer = gs.FirstPlayer} |> getDrawPhaseActions 

let gotoNextPhase gs =
    let gs' = gs |> cleanPhaseFlags |> cleanPhaseTriggers
    match gs.GamePhase with
    | Dynasty -> gs' |> gotoDrawPhase
    | _ -> gs'

let addDynastyPassTrigger gs =
    let pl1Pass gs = gs.Player1State |> hasPlayerFlag Passed
    let pl2Pass gs = gs.Player2State |> hasPlayerFlag Passed
    let bothPlPass gs = pl1Pass gs && pl2Pass gs
    let trigger = 
      { Name = "Pass trigger"
        Lifetime = Phase
        Condition = bothPlPass
        Action =  gotoNextPhase }
    {gs with Triggers = trigger :: gs.Triggers}

let gotoDynastyPhase gs = 
    gs
    |> revealAllDynastyCardsAtProvinces
    |> collectFateFromStronghold
    |> addDynastyPassTrigger
    |> getDynastyPhaseActions

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
    |> addWinConditionsTriggers