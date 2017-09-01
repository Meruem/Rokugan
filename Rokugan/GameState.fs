module GameState
open GameTypes

let chooseRandomPlayer () = 
    let rnd = System.Random()
    if rnd.Next(2) = 0 then Player1 else Player2

let drawCardsFromDeck n (deck : Card list) = 
    let c = min n deck.Length
    List.splitAt c deck

let drawCardFromDeck (deck : Card list) = 
    match deck with
    | card :: rest -> Some (card, rest)
    | _ -> None

let addHonor honor (playerState:PlayerState) = 
    { playerState with Honor = playerState.Honor + honor}

let shuffleDeck (deck:Card list) =
    deck

let recycleDynastyDiscard playerState =
    { playerState with 
        DynastyDiscard = []
        DynastyDeck = playerState.DynastyDiscard |> shuffleDeck }

let rec drawCardFromDynastyDeck (playerState : PlayerState) = 
    match drawCardFromDeck playerState.DynastyDeck with
    | Some (card, rest) -> card, { playerState with DynastyDeck = rest }
    | None -> playerState |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromDynastyDeck      


let createCard title player = {
    Title = title
    Owner = player
    States = [] }

let createProviceCard title player = {
    ProvinceCard = createCard title player
    State = ProvinceState.Hidden }

let createStrongholdCard title player = { StrongholdCard = createCard title player }

let initializePlayerState (initialConfig:InitialPlayerConfig) =
    let init card = createCard card initialConfig.Player 
    let conflictDeck = initialConfig.ConflictDeck |> List.map init
    let hand, conflictDeck' = drawCardsFromDeck 4 conflictDeck
    let dynastyDeck = initialConfig.DynastyDeck |> List.map init
    let dynastyHand, dynastyDeck' = drawCardsFromDeck 4 dynastyDeck
    let initProvince title = createProviceCard title initialConfig.Player
    let stronghold = CardRepository.getStrongholdCard initialConfig.Stonghold
    {
        DynastyDiscard = []
        ConflictDiscard = []
        Home = []
        Honor = stronghold.StartingHonor
        Fate = 0
        ConflictDeck = conflictDeck'
        DynastyDeck = dynastyDeck'
        Hand = hand
        DynastyInProvinces = dynastyHand |> List.map (fun c -> {c with States = [Hidden]})
        Stonghold = createStrongholdCard initialConfig.Stonghold initialConfig.Player
        StrongholdProvince =  initProvince initialConfig.StrongholdProvince
        Provinces = initialConfig.Provinces |> List.map  initProvince
    } 

let initializeGameState playerConfig1 playerConfig2 = 
    let firstPlayer = chooseRandomPlayer ()
    {
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
        { pState with DynastyInProvinces = pState.DynastyInProvinces |> List.map removeHiddenState }
    { gameState with 
        Player1State = removeHiddenFromPlayerState gameState.Player1State
        Player2State = removeHiddenFromPlayerState gameState.Player2State }

let addFateToPlayer fate playerState = 
    { playerState with Fate = playerState.Fate + fate }

let getPlayerState player gamestate = match player with | Player1 -> gamestate.Player1State | Player2 -> gamestate.Player2State

let hasEnoughFate playerState cardDef = cardDef.Cost <= playerState.Fate 

let playDynastyCard position player gameState =
    let state = getPlayerState player gameState
    let card = state.DynastyInProvinces.[position]
    let home = card :: state.Home
    let newCard, state' = drawCardFromDynastyDeck state
    let cardDef = CardRepository.getCharacterCard card.Title
    // add new dynasty card
    let (_, dynastyInProvinces') = 
        state.DynastyInProvinces 
        |> List.fold (fun (i, acc) d -> 
            let c = if i = position then newCard else d
            (i+1, List.append acc [c])) (0, [])
    let state'' = 
        {state' with 
            Home = home
            DynastyInProvinces = dynastyInProvinces'
            Fate = state'.Fate - cardDef.Cost}
    match player with
    | Player1 -> {gameState with Player1State = state''}
    | Player2 -> {gameState with Player2State = state''}
    

let getPlayableDynastyPositions playerState =
    playerState.DynastyInProvinces 
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
        |> List.map (fun pos ->  PlayCharacter (playerState.DynastyInProvinces.[pos].Title, playDynastyCard pos gameState.ActivePlayer))
    let actions' = PlayerAction.Pass (fun gs -> 
        let gs' = {gs with ActivePlayer = match gs.ActivePlayer with |Player1 -> Player2 |Player2 -> Player1}
        getDynastyPhaseActions gs') :: actions
    {gameState with AvailablePlayerActions = actions'}
    
