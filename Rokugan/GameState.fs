module GameState
open GameTypes
open System
open PlayerState

let changePlayerState player playerState gameState =
    match player with 
    | Player1 -> {gameState with Player1State = playerState}
    | Player2 -> {gameState with Player2State = playerState}

let initializeGameState playerConfig1 playerConfig2 = 
    let firstPlayer = Utils.chooseRandomPlayer ()
    {
        TurnNumber = 1
        ActivePlayer = firstPlayer
        Actions = []
        GamePhase = Dynasty
        FirstPlayer = firstPlayer
        Player1State = initializePlayerState playerConfig1
        Player2State = initializePlayerState playerConfig2  }

let revealAllDynastyCardsAtProvinces gameState =
    let removeHiddenState = Card.removeCardState Hidden
    let removeHiddenFromPlayerState pState =
        { pState with DynastyInProvinces = pState.DynastyInProvinces.Cards |> List.map removeHiddenState |> Zone }
    { gameState with 
        Player1State = removeHiddenFromPlayerState gameState.Player1State
        Player2State = removeHiddenFromPlayerState gameState.Player2State }

let getPlayerState player gamestate = match player with | Player1 -> gamestate.Player1State | Player2 -> gamestate.Player2State

let playDynastyCard position player gameState =
    let state = getPlayerState player gameState
    let card = state.DynastyInProvinces.Cards.[position]
    let home = card :: state.Home.Cards
    let newCard, state' = drawCardFromDynastyDeck state
    let cardDef = CardRepository.getCharacterCard card.Title
    // add new dynasty card
    let dynastyInProvinces' = 
        state.DynastyInProvinces.Cards 
        |> Utils.replaceListElement newCard position
    let state'' = 
        {state' with 
            Home = Zone home
            DynastyInProvinces = Zone dynastyInProvinces'
            Fate = state'.Fate - cardDef.Cost}
    gameState |> changePlayerState player state''    

let collectFateFromStronghold gameState =
    let stronghold (pState:PlayerState) = pState.Stonghold.StrongholdCard.Title |> CardRepository.getCard
    let fate pState =
        match (stronghold pState).Spec with 
        | Stronghold s -> s.FatePerRound
        | _ -> failwith "Stronghold card is not stronghold card"

    let p1Add = fate gameState.Player1State
    let p2Add = fate gameState.Player2State
    { gameState with 
        Player1State = (addFateToPlayer p1Add gameState.Player1State)    
        Player2State = (addFateToPlayer p2Add gameState.Player2State)  }

let switchActivePlayer gs =
    {gs with ActivePlayer = match gs.ActivePlayer with |Player1 -> Player2 |Player2 -> Player1}

let activePlayerState gs =
        match gs.ActivePlayer with
        | Player1 -> gs.Player1State
        | Player2 -> gs.Player2State

let rec getDynastyPhaseActions gameState =
    let playerState = activePlayerState gameState
    let actions = 
        getPlayableDynastyPositions playerState
        |> List.map (fun pos -> 
          { Action =  playDynastyCard pos gameState.ActivePlayer >> getDynastyPhaseActions >> switchActivePlayer
            Type = PlayCharacter playerState.DynastyInProvinces.Cards.[pos].Title })
    let actions' = 
        { Type = PlayerActionType.Pass 
          Action = getDynastyPhaseActions >> switchActivePlayer } :: actions
    { gameState with Actions = actions'}
    
let playAction n gameState =
    if n > gameState.Actions.Length then gameState
        else gameState.Actions.[n].Action gameState  