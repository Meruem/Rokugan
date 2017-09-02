module GameState
open GameTypes
open System

let changePlayerState player playerState gameState =
    match player with 
    | Player1 -> {gameState with Player1State = playerState}
    | Player2 -> {gameState with Player2State = playerState}

let drawCardFromDeck (deck : Deck) =
    let (Deck lst) = deck
    match lst with
    | card :: rest -> Some (card, Deck rest)
    | _ -> None

let addHonor honor (playerState:PlayerState) = 
    { playerState with Honor = playerState.Honor + honor}

let shuffleDeck (deck:Deck) =
    deck.Cards |> List.sortBy (fun c -> Guid.NewGuid ()) |> Deck

let zoneToDeck = function Zone l -> Deck l

let recycleDynastyDiscard playerState =
    { playerState with 
        DynastyDiscard = Zone []
        DynastyDeck = playerState.DynastyDiscard |> zoneToDeck |> shuffleDeck }

let recycleConflictDiscard playerState =
    { playerState with 
        ConflictDiscard = Zone []
        ConflictDeck = playerState.ConflictDiscard |> zoneToDeck |> shuffleDeck }

let rec drawCardFromDynastyDeck (playerState : PlayerState) = 
    match drawCardFromDeck playerState.DynastyDeck with
    | Some (card, rest) -> card, { playerState with DynastyDeck = rest }
    | None -> playerState |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromDynastyDeck      

let rec drawCardFromConflictDeck (playerState : PlayerState) = 
    match drawCardFromDeck playerState.ConflictDeck with
    | Some (card, rest) -> card, { playerState with ConflictDeck = rest }
    | None -> playerState |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromConflictDeck      

let createCard title player = {
    Title = title
    Owner = player
    States = [] }

let createProviceCard title player = {
    ProvinceCard = createCard title player
    State = ProvinceState.Hidden }

let createStrongholdCard title player = { StrongholdCard = createCard title player }

let getCardsFromDeck n (deck:Deck) = 
    let (Deck lst) = deck
    let hand, rest = List.splitAt 4 lst
    (Zone hand), (Deck rest)

let initializePlayerState (initialConfig:InitialPlayerConfig) =
    let init card = createCard card initialConfig.Player 
    let conflictDeck = initialConfig.ConflictDeck |> List.map init |> Deck
    let hand, conflictDeck' = getCardsFromDeck 4 conflictDeck
    let dynastyDeck = initialConfig.DynastyDeck |> List.map init |> Deck
    let dynastyHand, dynastyDeck' = getCardsFromDeck 4 dynastyDeck
    let initProvince title = createProviceCard title initialConfig.Player
    let stronghold = CardRepository.getStrongholdCard initialConfig.Stonghold
    {
        DynastyDiscard = Zone []
        ConflictDiscard = Zone []
        Home = Zone []
        Honor = stronghold.StartingHonor
        Fate = 0
        ConflictDeck = conflictDeck'
        DynastyDeck = dynastyDeck'
        Hand = hand
        DynastyInProvinces = dynastyHand.Cards |> List.map (fun c -> {c with States = [Hidden]}) |> Zone
        Stonghold = createStrongholdCard initialConfig.Stonghold initialConfig.Player
        StrongholdProvince =  initProvince initialConfig.StrongholdProvince
        Provinces = initialConfig.Provinces |> List.map  initProvince
    } 

let initializeGameState playerConfig1 playerConfig2 = 
    let firstPlayer = Utils.chooseRandomPlayer ()
    {
        TurnNumber = 1
        ActivePlayer = firstPlayer
        AvailablePlayerActions = []
        GamePhase = Dynasty
        FirstPlayer = firstPlayer
        Player1State = initializePlayerState playerConfig1
        Player2State = initializePlayerState playerConfig2  }

let removeCardState state (card:Card) =
     { card with States = card.States |> List.filter (fun s -> s <> state) }

let revealAllDynastyCardsAtProvinces gameState =
    let removeHiddenState = removeCardState Hidden
    let removeHiddenFromPlayerState pState =
        { pState with DynastyInProvinces = pState.DynastyInProvinces.Cards |> List.map removeHiddenState |> Zone }
    { gameState with 
        Player1State = removeHiddenFromPlayerState gameState.Player1State
        Player2State = removeHiddenFromPlayerState gameState.Player2State }

let addFateToPlayer fate playerState = 
    { playerState with Fate = playerState.Fate + fate }

let getPlayerState player gamestate = match player with | Player1 -> gamestate.Player1State | Player2 -> gamestate.Player2State

let hasEnoughFate playerState cardDef = cardDef.Cost <= playerState.Fate 

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

let getPlayableDynastyPositions playerState =
    playerState.DynastyInProvinces.Cards 
    |> List.mapi (fun i card -> 
        let cardDef = CardRepository.getDynastyCard card.Title
        let enoughFate = 
            match cardDef with 
            | Holding _ -> false 
            | DynastyCardDef.Character c -> c.Cost <= playerState.Fate
        let hidden = List.contains Hidden card.States
        if enoughFate && (not hidden) then i else -1)
    |> List.filter (fun i -> i <> -1)

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

let rec getDynastyPhaseActions gameState =
    let playerState = 
        match gameState.ActivePlayer with
        | Player1 -> gameState.Player1State
        | Player2 -> gameState.Player2State
    let actions = 
        getPlayableDynastyPositions playerState
        |> List.map (fun pos -> 
          { Action =  playDynastyCard pos gameState.ActivePlayer
            Type = PlayCharacter playerState.DynastyInProvinces.Cards.[pos].Title })
    let actions' = 
        { Type = PlayerActionType.Pass 
          Action = (fun gs -> 
                let gs' = {gs with ActivePlayer = match gs.ActivePlayer with |Player1 -> Player2 |Player2 -> Player1}
                getDynastyPhaseActions gs') } :: actions
    { gameState with AvailablePlayerActions = actions'}
    
let playAction n gameState =
    if n > gameState.AvailablePlayerActions.Length then gameState
        else gameState.AvailablePlayerActions.[n].Action gameState  