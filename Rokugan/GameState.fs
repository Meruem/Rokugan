module GameState

open RokuganShared
open GameTypes

open System
open PlayerState

// --------------- Game state helpers --------------------
 
let changePlayerState player playerStateChange gs =
    match player with
    | Player1 -> {gs with Player1State = gs.Player1State |> playerStateChange}
    | Player2 -> {gs with Player2State = gs.Player2State |> playerStateChange}

let changeActivePlayerState playerStateChange gs = changePlayerState gs.ActivePlayer playerStateChange gs
let changeOtherPlayerState playerStateChange gs = changePlayerState (otherPlayer gs.ActivePlayer) playerStateChange gs

let changeBothPlayerState playerStateChange gs = 
  { gs with 
        Player1State = gs.Player1State |> playerStateChange
        Player2State = gs.Player2State |> playerStateChange }

let playerState player gs =
    match player with
    | Player1 -> gs.Player1State
    | Player2 -> gs.Player2State
 
let activePlayerState gs = playerState gs.ActivePlayer gs

let otherPlayerState gs = playerState (otherPlayer gs.ActivePlayer) gs

let changeCard change card (gs: GameState) =
    changePlayerState card.Owner (fun ps -> ps |> changePlayerCard change card.Id) gs

let changeCards change cards gs =
    cards |> List.fold (fun agg c -> changeCard change c agg) gs     


// -------------- Game state queries ------------------

let hasPlayerPassed player gs =
    match player with
    | Player1 -> hasPassed gs.Player1State
    | Player2 -> hasPassed gs.Player2State

let topConflictCards n player gs = 
    let ps = gs |> playerState player
    ps.ConflictDeck.Cards 
    |> List.fold (fun (acc: Card list) c -> if acc.Length < n then c::acc else acc) []



// -------------- Game state modifiers --------------

let addSecondPlayer1Fate gs =
    let otherPl = otherPlayer gs.FirstPlayer
    let add1Fate (ps:PlayerState) = {ps with Fate = ps.Fate + 1}
    gs |> changePlayerState otherPl add1Fate

let switchActivePlayer gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPlayerPassed otherPl gs then gs
    else { gs with ActivePlayer = otherPl}

let passActive = changeActivePlayerState passPlayer

let cleanPhaseFlags (gs:GameState) =
    { gs with 
        Player1State = cleanPhaseFlags gs.Player1State
        Player2State = cleanPhaseFlags gs.Player2State }

let cleanPass gs = gs |> changeBothPlayerState PlayerState.cleanPass

let removeFateFromRing ring (gs:GameState) =
    { gs with Rings = gs.Rings |> Utils.replaceListElement { ring with Fate = 0 } (fun r -> r.Element = ring.Element) }

let honor = changeCard Card.honor
let dishonor = changeCard Card.dishonor

let nextRound (gs:GameState) = 
    {gs with 
        TurnNumber = gs.TurnNumber + 1
        FirstPlayer = otherPlayer gs.FirstPlayer}

let drawDynasty position player gs = 
    let ps' = drawCardFromDynastyDeck position (playerState player gs)
    gs |> changePlayerState player (fun _ -> ps')

let cleanDeclaredConflicts (gs:GameState) =
    let clearConflict (ps:PlayerState) = {ps with DeclaredConflicts = []}
    gs 
    |> changePlayerState Player1 clearConflict
    |> changePlayerState Player2 clearConflict

let addCardEffect id card lifetime effect gs =
    let newEffect = 
      { CardEffect.Id = id
        Card = card
        Lifetime = lifetime
        Type = effect }
    {gs with CardEffects = newEffect :: gs.CardEffects }

let removeCardEffect id gs =
    {gs with CardEffects = gs.CardEffects |> List.filter (fun ce -> ce.Id <> id)}

let cleanEffectsByLifetime lifetime gs =
    gs.CardEffects 
    |> List.filter (fun ce -> ce.Lifetime = lifetime)
    |> List.fold (fun gs' ce -> gs' |> removeCardEffect ce.Id) gs


// --------------------------- message handlers ------------------------------

let onChangePhase phase (gs:GameState) = 
    {gs with GamePhase = phase; ActivePlayer = gs.FirstPlayer}     
    |> cleanPhaseFlags 
    |> cleanDeclaredConflicts
    |> cleanEffectsByLifetime Lifetime.Phase
     

let onRemoveCardState card state (gs:GameState) =
    gs |> changeCard (Card.removeCardState state) card


let onAddFate player amount gs =
    gs |> changePlayerState player (addFate amount)

let onSwitchActivePlayer = switchActivePlayer

let onAddFateToCard fate card = changeCard (Card.addFateOnCard fate) card

let onPlayerBid player amount = changePlayerState player (changeBid amount)

let onCleanBids = changeBothPlayerState cleanBid 
let onAddHonor player amount = changePlayerState player (addHonor amount) 
let onDrawConflictCard player amount = changePlayerState player (drawConflictCards amount)
let onRevealProvince = changeCard Card.revealProvince 
let onCollectFateFromRing player (ring:Ring) = changePlayerState player (addFateToPlayer ring.Fate) >> removeFateFromRing ring
let onDiscardRandomConflict player =  changePlayerState player discardRandomConflictCard

let onCardBow = changeCard Card.bow
let onCardReady = changeCard Card.ready
let onCardHonor = changeCard Card.honor
let onCardDishonor = changeCard Card.dishonor
let onBreakProvince = changeCard Card.breakProvince

let onDiscardCardFromPlay = changeCard (Card.move DynastyDiscard) 

let onNextRound gs = 
    gs 
    |> nextRound
    |> cleanEffectsByLifetime Lifetime.Round

let onSetActivePlayer player (gs:GameState) = { gs with ActivePlayer = player }

let onAddCardEffect = 
    let id = Utils.newId ()
    addCardEffect id 

let onRemoveCardEffect = removeCardEffect

let onRevealCard card gs = { gs with RevealedCards = card :: gs.RevealedCards} 
let onCleanRevealedCards gs = {gs with RevealedCards = []}
let onAddTrigger = Triggers.addTrigger 

let onRemoveTrigger = Triggers.removeTrigger

let onDeckShuffle player deckType (gs:GameState) =
    gs |> changePlayerState 
        player
        (fun ps ->
            match deckType with
            | Dynasty -> {ps with DynastyDeck = Deck.shuffleDeck ps.DynastyDeck}
            | Conflict -> {ps with ConflictDeck = Deck.shuffleDeck ps.ConflictDeck})

let onPutCardFromDeckToHand id (gs:GameState) =
    let cardM = gs.Cards |> List.tryFind (fun c -> c.Id = id)
    match cardM with 
    | None -> printfn "Card not found"; gs
    | Some card -> 
        let card' = { card with Zone = Hand }
        gs 
        |> changePlayerState card.Owner
            (fun ps -> 
                match card.Zone with 
                | ZoneName.ConflictDeck -> 
                    { ps with 
                        ConflictDeck = Deck (ps.ConflictDeck.Cards |> List.filter (fun c -> c.Id <> id))
                        CardsInPlay = ps.CardsInPlay |> Map.add id card' }
                | _ -> printfn "not in conflict deck"; ps) 
