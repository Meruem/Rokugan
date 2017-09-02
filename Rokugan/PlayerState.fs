module PlayerState

open GameTypes

let otherPlayer player = if player = Player1 then Player2 else Player1
let hasPlayerFlag flag playerState =
    playerState.Flags |> List.exists (fun ps -> ps.Flag = flag)

let hasPassed = hasPlayerFlag PlayerFlagEnum.Passed

let pass ps = {ps with Flags = { Lifetime = Phase; Flag = Passed } :: ps.Flags}

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
        Flags = []
    }   

let addFateToPlayer fate playerState = 
    { playerState with Fate = playerState.Fate + fate }      

let getPlayableDynastyPositions playerState =
    playerState.DynastyInProvinces.Cards 
    |> List.mapi (fun i card -> 
        let cardDef = CardRepository.getDynastyCard card.Title
        let remainingFate = 
            match cardDef with 
            | Holding _ -> -1 
            | DynastyCardDef.Character c -> playerState.Fate - c.Cost
        let hidden = List.contains Hidden card.States
        if remainingFate >= 0 && (not hidden) then (i, remainingFate) else (-1,0))
    |> List.filter (fun (i, _) -> i <> -1)
 
let cleanPhaseFlags playerState = 
    let flags = playerState.Flags |> List.filter (fun f -> f.Lifetime <> Phase) 
    { playerState with Flags = flags }   