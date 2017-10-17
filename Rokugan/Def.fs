module Def

open RokuganShared
open GameTypes
open CardDef
open CardDef.CardDef
module PA = PlayerActions

let inPlay = fun cardId (gs:GameState) -> Card.isInPlay (gs.Card cardId)
let onDecklaredAsAttacker (id:CardId) cmd _ = 
    match cmd with
    | DeclareAttacker c -> c = id 
    | _ -> false

let onCardPlayed (id:CardId) cmd gs =
    match cmd with
    | PlayDynasty (c, pos) -> c = id
    // missing character from hand, attachment    
    | _ -> false

let onWonConflict (id:CardId) cmd gs =
    match cmd with
    | ConflictEnd (Some state) -> state.Winners |> List.exists (fun w -> w = id)
    | _ -> false

let onLostConflict (id:CardId) cmd gs =
    match cmd with
    | ConflictEnd (Some state) -> state.Loosers |> List.exists (fun l -> l = id)
    | _ -> false

//----------------- Actions --------------------------

let covertEffect cardId (gs:GameState) =
    let activatingPlayer = (gs.Card cardId).Owner
    let onTargetSelected (card:Card) = 
        act <| fun gs -> [AddCardEffect (card.Id, Lifetime.Conflict, CannotBlock)]
    let onPass = none ()
    (PA.chooseCharacterInPlayOrPass activatingPlayer "Covert target" onTargetSelected onPass) gs 

let covertTrigger =
    tName "covert"
    @?+ tLifetime Game
    @?+ tCondition onDecklaredAsAttacker
    @?+ tEffect (fun cardId cmd -> 
                     changes [Debug "covert activated"]
                     >+> playerActions (covertEffect cardId) "Choose target for convert: ")
let covert = trigger covertTrigger


let pernamentCardEffect effect =
    trigger
        (tName "add pernament card effect"
        @?+ tLifetime Game
        @?+ tCondition onCardPlayed
        @?+ tEffect (fun cardId cmd ->
            act (fun gs -> [AddCardEffect (cardId, Lifetime.Game, effect)])))


let cannotBlock = pernamentCardEffect CannotBlock

let pride = 
    trigger 
        (tName "pride win"
        @?+ tLifetime Game
        @?+ tCondition onWonConflict
        @?+ tEffect (fun cardId cmd -> changes [Honor cardId]))
    @+ trigger            
        (tName "pride loss"
        @?+ tLifetime Game
        @?+ tCondition onLostConflict
        @?+ tEffect (fun cardId cmd -> changes [Dishonor cardId]))
    
