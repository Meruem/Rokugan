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
            (fun thisCard -> 
                let onTargetSelected card = 
                    let cardTrigger = 
                        covertTrigger
                        @?+ tCondition 
                            (fun card cmd gs ->
                                match gs.AttackState with 
                                | Some state -> state.Ring.Element = Element.Water
                                | None -> false )
                    let newTrigger = Triggers.fromCardTrigger card covertTrigger
                    changes [AddTrigger newTrigger]
                let onPass = none ()
                playerActions (PA.chooseCharacterInPlayOrPass thisCard.Owner "Gain covert in water conflict" onTargetSelected onPass) "Choose target for water covert: "))


let ``Agasha Swordsmith`` =
    title "Agasha Swordsmith"
    @+ clan Clan.Dragon
    @+ dynastychar 2 (Some 1) (Some 2) 1 [Shugenja; Fire]
    @+ action
        (aName "Search for attachment"
         @!+ aCondition inPlay
         @!+ aEffect
            (fun thisCard -> 
                let passAction = changes [CleanRevealedCards; Shuffle (thisCard.Owner, DeckType.Conflict)]
                let afterChoice (card:Card) = 
                    changes [PutCardFromDeckToHand card.Id]
                    >+> passAction
                let onlyAttachment  = function | Card.Attachment _ -> true | _ -> false 
                act (topConflictCards 5 thisCard.Owner >> List.map RevealCard)
                >+> playerActions (PA.chooseRevealedCardOrPassCond onlyAttachment thisCard.Owner "Choose revealed card" afterChoice passAction) "Choose attachment: "))

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
            (fun card cmd gs -> 
                match cmd with
                | PlayDynasty (c, pos) -> card.Id = c.Id
                | _ -> false)
        @?+ tEffect (fun card -> none ())) 


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
   [ ``Akodo Gunsō`` 
     ``Adept of the Waves``
     ``Artisan Academy``
     ``Admit Defeat``
     ``Ancestral Daishō``
     ``Defend the Wall``
     ``Golden Plains Outpost`` ]
   |> List.map addCore
