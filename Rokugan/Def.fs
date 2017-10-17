module Def

open RokuganShared
open GameTypes
open CardDef
open CardDef.CardDef
module PA = PlayerActions

let inPlay = fun card _ -> Card.isInPlay card
let onDecklaredAsAttacker (card:Card) cmd _ = 
    match cmd with
    | DeclareAttacker c -> c.Id = card.Id 
    | _ -> false

let onCardPlayed (card:Card) cmd gs =
    match cmd with
    | PlayDynasty (c, pos) -> c.Id = card.Id
    // missing character from hand, attachment    
    | _ -> false

let onWonConflict (card:Card) cmd gs =
    match cmd with
    | ConflictEnd (Some state) -> state.Winners |> List.exists (fun w -> w.Id = card.Id)
    | _ -> false

let onLostConflict (card:Card) cmd gs =
    match cmd with
    | ConflictEnd (Some state) -> state.Loosers |> List.exists (fun l -> l.Id = card.Id)
    | _ -> false

//----------------- Actions --------------------------

let covertEffect activatingPlayer =
    let onTargetSelected card = 
        act <| fun gs -> [AddCardEffect (card, Lifetime.Conflict, CannotBlock)]
    let onPass = none ()
    playerActions (PA.chooseCharacterInPlayOrPass activatingPlayer "Covert target" onTargetSelected onPass) "Choose target for convert: "

let covertTrigger =
    tName "covert"
    @?+ tLifetime Game
    @?+ tCondition onDecklaredAsAttacker
    @?+ tEffect (fun card -> 
                     changes [Debug "covert activated"]
                     >+> covertEffect card.Owner)
let covert = trigger covertTrigger


let pernamentCardEffect effect =
    trigger
        (tName "add pernament card effect"
        @?+ tLifetime Game
        @?+ tCondition onCardPlayed
        @?+ tEffect (fun card ->
            changes [AddCardEffect (card, Lifetime.Game, effect)]))


let cannotBlock = pernamentCardEffect CannotBlock

let pride = 
    trigger 
        (tName "pride win"
        @?+ tLifetime Game
        @?+ tCondition onWonConflict
        @?+ tEffect (fun card -> changes [Honor card]))
    @+ trigger            
        (tName "pride loss"
        @?+ tLifetime Game
        @?+ tCondition onLostConflict
        @?+ tEffect (fun card -> changes [Dishonor card]))
    
