module CoreCards

open RokuganShared
open GameTypes
open CardDef
open CardDef.CardDef
open Def
module PA = PlayerActions
open GameState

let ``Adept of the Waves`` =
    title "Adept of the Waves" 
    @+ clan Clan.Phoenix
    @+ dynastychar 2 (Some 2) (Some 2) 2 [Shugenja; Water]
    @+ action 
        (aName "Give covert in water"
        @!+ aCondition inPlay
        @!+ aEffect 
            (fun thisCardId -> 
                let onTargetSelected (card:Card) = 
                    let cardTrigger = 
                        covertTrigger
                        @?+ tCondition 
                            (fun card cmd gs ->
                                match gs.AttackState with 
                                | Some state -> state.Ring.Element = Element.Water
                                | None -> false )
                    let newTrigger = Triggers.fromCardTrigger card.Id covertTrigger
                    changes [AddTrigger newTrigger]
                let onPass = none ()
                let getActions (gs:GameState) =
                    let card = gs.Card thisCardId
                    PA.chooseCharacterInPlayOrPass card.Owner "Gain covert in water conflict" onTargetSelected onPass gs

                playerActions getActions 
                    "Choose target for water covert: " ))


let ``Agasha Swordsmith`` =
    title "Agasha Swordsmith"
    @+ clan Clan.Dragon
    @+ dynastychar 2 (Some 1) (Some 2) 1 [Shugenja; Fire]
    @+ action
        (aName "Search for attachment"
         @!+ aCondition inPlay
         @!+ aEffect
            (fun thisCardId -> 
                let passAction = 
                    act <| fun (gs:GameState) ->
                        let card = gs.Card thisCardId
                        [CleanRevealedCards; Shuffle (card.Owner, DeckType.Conflict)]
                let afterChoice (card:Card) = 
                    changes [PutCardFromDeckToHand card.Id]
                    >+> passAction
                let onlyAttachment  = function | Card.Attachment _ -> true | _ -> false 
                act (fun gs ->
                    gs 
                    |> topConflictCards 5 (gs.Card thisCardId).Owner
                    |> List.map RevealCard)
                >+> playerActions
                    (fun gs -> 
                        let card = gs.Card thisCardId
                        PA.chooseRevealedCardOrPassCond onlyAttachment card.Owner "Choose revealed card" afterChoice passAction gs) "Choose attachment: "))

let ``Aggressive Moto`` =
    title "Aggressive Moto"
    @+ clan Clan.Unicorn
    @+ dynastychar 2 (Some 3) (Some 0) 0 [Bushi; Cavalry]
    @+ cannotBlock

let ``Akodo Gunsō`` = 
    title "Akodo Gunsō" 
    @+ clan Clan.Lion
    @+ dynastychar 2 (Some 2)  (Some 1) 2  [Bushi]
    @+ pride 
    @+ trigger 
        (tName "Replenish dynasty"
        @?+ tLifetime Game
        @?+ tCondition 
            (fun cardId cmd gs -> 
                match cmd with
                | PlayDynasty (c, pos) -> cardId = c
                | _ -> false)
        @?+ tEffect (fun cardId cmd -> none ())) 


let ``Artisan Academy`` =
    title "Artisan Academy"
    @+ clan Clan.Crane
    @+ holding 1 [Academy]

let ``Admit Defeat`` =
    title "Admit Defeat"
    @+ clan Clan.Crane
    @+ event 1

let ``Ancestral Daishō`` = 
    title "Ancestral Daishō"
    @+ clan Clan.Dragon
    @+ attachment 1 2 0 [Weapon]

let ``Defend the Wall`` = 
    title "Defend the Wall"
    @+ clan Clan.Crab
    @+ province 4 Earth

let ``Golden Plains Outpost`` =
    title "Golden Plains Outpost"
    @+ clan Clan.Unicorn
    @+ stronghold 0 10 7 10

let addCore def = def @+ cardset Core


let coreCards =
   [ ``Adept of the Waves``
     ``Agasha Swordsmith``
     ``Aggressive Moto``
     ``Akodo Gunsō`` 
     ``Artisan Academy``
     ``Admit Defeat``
     ``Ancestral Daishō``
     ``Defend the Wall``
     ``Golden Plains Outpost`` ]
   |> List.map addCore
