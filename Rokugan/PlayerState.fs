module PlayerState

open GameTypes

let zoneToDeck = function Zone l -> Deck l

let addHonor honor (playerState:PlayerState) = 
    { playerState with Honor = playerState.Honor + honor}

let recycleDynastyDiscard playerState =
    { playerState with 
        DynastyDiscard = Zone []
        DynastyDeck = playerState.DynastyDiscard |> zoneToDeck |> Deck.shuffleDeck }

let recycleConflictDiscard playerState =
    { playerState with 
        ConflictDiscard = Zone []
        ConflictDeck = playerState.ConflictDiscard |> zoneToDeck |> Deck.shuffleDeck }

let rec drawCardFromDynastyDeck (playerState : PlayerState) = 
    match Deck.drawCardFromDeck playerState.DynastyDeck with
    | Some (card, rest) -> card, { playerState with DynastyDeck = rest }
    | None -> playerState |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromDynastyDeck      

let rec drawCardFromConflictDeck (playerState : PlayerState) = 
    match Deck.drawCardFromDeck playerState.ConflictDeck with
    | Some (card, rest) -> card, { playerState with ConflictDeck = rest }
    | None -> playerState |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromConflictDeck      

let initializePlayerState (initialConfig:InitialPlayerConfig) =
    let init card = Card.createCard card initialConfig.Player 
    let conflictDeck = initialConfig.ConflictDeck |> List.map init |> Deck
    let hand, conflictDeck' = Deck.getCardsFromDeck 4 conflictDeck
    let dynastyDeck = initialConfig.DynastyDeck |> List.map init |> Deck
    let dynastyHand, dynastyDeck' = Deck.getCardsFromDeck 4 dynastyDeck
    let initProvince title = Card.createProviceCard title initialConfig.Player
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
        Stonghold = Card.createStrongholdCard initialConfig.Stonghold initialConfig.Player
        StrongholdProvince =  initProvince initialConfig.StrongholdProvince
        Provinces = initialConfig.Provinces |> List.map  initProvince
    }   

let addFateToPlayer fate playerState = 
    { playerState with Fate = playerState.Fate + fate }      

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
    