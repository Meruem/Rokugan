module Ring

open RokuganShared
open GameTypes
open GameState
open PlayerState
open PlayerActions

let createRing element = {Element = element; State = Unclaimed; Fate = 0}
let changeRing ringElement change gs =
    let rings' = [ for ring in gs.Rings do if ring.Element = ringElement then yield (change ring) else yield ring ]
    {gs with Rings = rings'}

let claimRing (ring:Ring) player = changeRing ring.Element (fun r -> {r with State = Claimed player })
let contestRing (ring:Ring) = changeRing ring.Element (fun r -> { r with State = Contested})
let returnRing (ring:Ring) = changeRing ring.Element (fun r -> { r with State = Unclaimed })

let isUnclaimed (ring:Ring) = ring.State = Unclaimed

let resolveFireRing attacker =
    let honor = "Honor character"
    let dishonor = "Dishonor character"
    let honorChar (char:Card) = 
        changes [Honor char.Id] 
    let dishonorChar (char:Card) =
        changes [Dishonor char.Id] 
    let effectChosen effect =
        if effect = honor then
            playerActions (chooseCharacterInPlay attacker "Honor character" honorChar) "Choose character to honor: "
        else 
            playerActions (chooseCharacterInPlay attacker "Dishonor character" dishonorChar) "Choose character to dishonor: "
    playerActions (fun _ -> choice attacker "Fire ring effect" [honor; dishonor] effectChosen) "Choose honor or dishonor: "

let resolveAirRing attacker = 
    let plus2Honor = "+2 Honor"
    let take1Honor = "Take 1 honor"
    let effectChosen effect = 
        changes 
            (if effect = plus2Honor then 
                [AddHonor (attacker, 2)]
            else 
                [AddHonor (attacker, 1)
                 AddHonor (otherPlayer attacker, -1)])
    playerActions (fun _ -> choice attacker "Air ring effect" [plus2Honor; take1Honor] effectChosen) "Choose +1/-1 or +2 honor: "

let resolveEarthRing attacker  =
    changes
        [DrawConflictCard (attacker, 1)
         DiscardRandomConflict (otherPlayer attacker)]

let resolveVoidRing attacker =
    let remove1fate (card:Card) = 
        changes [AddFateOnCard (card.Id, -1)] 
    playerActions (chooseCharacter attacker Card.hasFate "Remove 1 fate from character" remove1fate) "Choose character to remove 1 fate: "

let resolveWaterRing attacker  =
    let bow = "Bow character without fate"
    let ready = "Ready character"
    let bowChar (char:Card) = 
        changes [Bow char.Id]
    let readyChar (char:Card) =
        changes [Ready char.Id]
    let effectChosen effect =
        if effect = bow then
            playerActions (chooseCharacter attacker (Card.hasFate >> not) "Bow character" bowChar) "Choose character to bow: "
        else playerActions (chooseCharacterInPlay attacker "Ready character" readyChar) "Choose character to ready: "
    playerActions (fun _ -> choice attacker "Water ring effect" [bow; ready] effectChosen) "Choose ready or bow: "

let addFate amount (ring:Ring) = {ring with Fate = ring.Fate + amount}


// ---------------------- message handlers -----------------------------

let onAddFateOnRing (ring:Ring) amount = changeRing ring.Element (addFate amount)
let onClaimRing = claimRing
let onContestRing = contestRing
let onReturnRing = returnRing