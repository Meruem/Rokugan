module PlayerState

open GameTypes
open System.Security.Policy

open System

let otherPlayer player = if player = Player1 then Player2 else Player1
let hasPlayerFlag flag playerState =
    playerState.Flags |> List.exists (fun ps -> ps.Flag = flag)

let changeBid bid playerState = { playerState with Bid = Some bid }

let hasPassed = hasPlayerFlag PlayerFlagEnum.Passed

let pass ps = {ps with Flags = { Lifetime = Phase; Flag = Passed } :: ps.Flags}

let addHonor honor (playerState:PlayerState) = 
    { playerState with Honor = playerState.Honor + honor}

let splitZoneAndOthers zone ps = 
    ps.CardsInPlay |> Map.toList |> Utils.listSplitBy (fun (_,c) -> c.Zone = zone)

let recycleDynastyDiscard ps =
    let (discard, other) = splitZoneAndOthers DynastyDiscard ps
    { ps with 
        CardsInPlay = other |> Map.ofList
        DynastyDeck = discard |> List.map (fun (_,c) -> {c with Zone = DynastyDeck}) |> Deck |> Deck.shuffleDeck }

let recycleConflictDiscard ps =
    let (discard, other) = splitZoneAndOthers ConflictDiscard ps
    { ps with 
        CardsInPlay = other |> Map.ofList
        ConflictDeck = discard |> List.map (fun (_,c) -> {c with Zone = ConflictDeck}) |> Deck |> Deck.shuffleDeck }

let rec drawCardFromDynastyDeck (playerState : PlayerState) = 
    match Deck.drawCardFromDeck playerState.DynastyDeck with
    | Some (card, rest) -> card, { playerState with DynastyDeck = rest }
    | None -> playerState |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromDynastyDeck      

let rec drawCardFromConflictDeck (ps : PlayerState) = 
    match Deck.drawCardFromDeck ps.ConflictDeck with
    | Some (card, rest) -> 
        { ps with 
            ConflictDeck = rest
            CardsInPlay = ps.CardsInPlay |> Map.add card.Id {card with Zone = Hand} }
    | None -> ps |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromConflictDeck      

let drawConflictCards n playerState = [1..n] |> List.fold (fun pstate i -> pstate |> drawCardFromConflictDeck) playerState

let initializePlayerState (initialConfig:InitialPlayerConfig) =
    let init zone title = Card.createCard title initialConfig.Player zone
    let conflictDeck = initialConfig.ConflictDeck |> List.map (init ConflictDeck) |> Deck
    let hand, conflictDeck' = Deck.getCardsFromDeck 4 conflictDeck
    let dynastyDeck = initialConfig.DynastyDeck |> List.map (init DynastyDeck) |> Deck
    let dynastyHand, dynastyDeck' = Deck.getCardsFromDeck 4 dynastyDeck
    let initProvince i title = Card.createProviceCard title initialConfig.Player i
    let initStrongholdProvince title = Card.createStrongholdProvinceCard title initialConfig.Player
    let strongholdCard = Card.createStrongholdCard initialConfig.Stonghold initialConfig.Player
    let stronghold = CardRepository.getStrongholdCard initialConfig.Stonghold
    {
        Bid = None
        Honor = stronghold.StartingHonor
        Fate = 0
        ConflictDeck = conflictDeck'
        DynastyDeck = dynastyDeck'
        CardsInPlay = 
          [ hand |> List.map (fun c -> {c with Zone = Hand })  
            [strongholdCard] 
            dynastyHand |> List.mapi (fun i c -> { c with Zone = DynastyInProvinces i})
            initialConfig.Provinces |> List.mapi initProvince
            [initStrongholdProvince initialConfig.StrongholdProvince]]
            |> List.concat |> List.map (fun c -> c.Id,c) |> Map.ofList
        Flags = []
        DeclaredConflicts = []
    }   

let addFateToPlayer fate (playerState:PlayerState) = 
    { playerState with Fate = playerState.Fate + fate }      

let getPlayableDynastyPositions (ps:PlayerState) =
    ps.DynastyInProvinces 
    |> List.map (fun card -> 
        let cardDef = CardRepository.getDynastyCard card.Title
        let nr = 
            match card.Zone with
            | DynastyInProvinces i ->  i
            | _ -> -1
        let remainingFate = 
            match cardDef with 
            | Holding _ -> -1 
            | DynastyCardDef.Character c -> ps.Fate - c.Cost
        let hidden = card |> Card.isHidden
        if remainingFate >= 0 && (not hidden) then (nr, remainingFate) else (-1,0))
    |> List.filter (fun (i, _) -> i <> -1)
 
let cleanPhaseFlags playerState = 
    let flags = playerState.Flags |> List.filter (fun f -> f.Lifetime <> Phase) 
    { playerState with Flags = flags }   

let changePlayerCard stateChange card ps =
    let cardsInPlay = ps.CardsInPlay |> Map.add card.Id (stateChange card)
    {ps with CardsInPlay = cardsInPlay}

let changePlayerCards cards stateChange (ps:PlayerState) =
    cards |> List.fold (fun agg c -> changePlayerCard stateChange c agg) ps     

let revealProvince province = changePlayerCard Card.revealProvince province

let addCardToPlay card zone ps = 
    let card' = {card with Zone = zone}
    { ps with CardsInPlay = ps.CardsInPlay |>  Map.add card.Id card' } 

let addFate fate (ps:PlayerState) = { ps with Fate = ps.Fate + fate} 

let charactersInPlay ps =
    ps.CardsInPlay 
    |> Map.filter (fun id card -> Card.character card |> Option.isSome)
    |> Map.toList
    |> List.map (fun (id, char) -> char)

let cardsByCondition cond ps =
    ps.CardsInPlay
    |> Map.filter (fun id card -> cond card)
    |> Map.toList
    |> List.map (fun (id, char) -> char)

let dynastyCardAtPosition position (state:PlayerState) =
    state.DynastyInProvinces 
    |> List.find (fun c -> match c.Zone with | DynastyInProvinces n -> n = position | _ -> false)

let honorCard = changePlayerCard Card.honorCard
let dishonorCard = changePlayerCard Card.dishonorCard

let discardCard = changePlayerCard Card.discardConflict

let discardRandomConflictCard (ps:PlayerState) =
    let rnd = Random ()
    if ps.Hand.Length = 0 then ps
    else ps |> discardCard ps.Hand.[rnd.Next(ps.Hand.Length)]