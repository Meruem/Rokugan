module GameState

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


// ------------- Actions operations -------------
let setActions actionList gs =
    {gs with Actions = actionList}

let addActions actionList gs =
    { gs with Actions = List.append gs.Actions actionList}

// Set game state actions
let (>!=>) gs actions = setActions actions gs

// Add game state actions
let (>+=>) gs actions = addActions actions gs

// -------------- Game state queries

let hasPlayerPassed player gs =
    match player with
    | Player1 -> hasPassed gs.Player1State
    | Player2 -> hasPassed gs.Player2State


// -------------- Game state modifiers --------------

let addSecondPlayer1Fate gs =
    let otherPl = otherPlayer gs.FirstPlayer
    let add1Fate (ps:PlayerState) = {ps with Fate = ps.Fate + 1}
    gs |> changePlayerState otherPl add1Fate

let switchActivePlayer gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPlayerPassed otherPl gs then gs
    else { gs with ActivePlayer = otherPl}

let passActive = changeActivePlayerState pass

let cleanPhaseFlags gs =
    { gs with 
        Player1State = cleanPhaseFlags gs.Player1State
        Player2State = cleanPhaseFlags gs.Player2State }

let removeFateFromRing ring gs =
    { gs with Rings = gs.Rings |> Utils.replaceListElement { ring with Fate = 0 } (fun r -> r.Element = ring.Element) }

let honor = changeCard Card.honor
let dishonor = changeCard Card.dishonor

let nextRound gs = {gs with TurnNumber = gs.TurnNumber + 1}

let drawDynasty position player gs = 
    let ps' = drawCardFromDynastyDeck position (playerState player gs)
    gs |> changePlayerState player (fun _ -> ps')

let cleanDeclaredConflicts (gs:GameState) =
    let clearConflict ps = {ps with DeclaredConflicts = []}
    gs 
    |> changePlayerState Player1 clearConflict
    |> changePlayerState Player2 clearConflict

let onChangePhase phase gs = 
    {gs with GamePhase = phase; ActivePlayer = gs.FirstPlayer}     
    |> cleanPhaseFlags |> cleanDeclaredConflicts

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
let onCollectFateFromRing player ring = changePlayerState player (addFateToPlayer ring.Fate) >> removeFateFromRing ring
let onDiscardRandomConflict player =  changePlayerState player discardRandomConflictCard

let onCardBow = changeCard Card.bow
let onCardReady = changeCard Card.ready
let onCardHonor = changeCard Card.honor
let onCardDishonor = changeCard Card.dishonor
let onBreakProvince = changeCard Card.breakProvince

let onDiscardCardFromPlay = changeCard (Card.move DynastyDiscard) 