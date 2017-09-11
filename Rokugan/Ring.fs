module Ring

open GameTypes
open GameState
open PlayerState
open Actions

let createRing element = {Element = element; State = Unclaimed; Fate = 0}
let changeRing ringElement change gs =
    let rings' = [ for ring in gs.Rings do if ring.Element = ringElement then yield (change ring) else yield ring ]
    {gs with Rings = rings'}

let claimRing ring player = changeRing ring.Element (fun r -> {r with State = Claimed player })
let contestRing ring = changeRing ring.Element (fun r -> { r with State = Contested})
let returnRing ring = changeRing ring.Element (fun r -> { r with State = Unclaimed })

let resolveFireRing attacker next gs =
    let honorWithFire next char (gs:GameState) = 
        let dishonorWithFire next char gs =
            gs |> dishonor char |> next
        gs 
        |> honor char 
        >!=> chooseCharacterInPlay attacker "Choose character to dishonor" (dishonorWithFire next) gs
    gs >!=> chooseCharacterInPlay attacker "Choose character to honor" (honorWithFire next) gs

let resolveAirRing attacker next gs = 
    let plus2Honor = "+2 Honor"
    let take1Honor = "Take 1 honor"
    let effectChosen effect gs = 
        if effect = plus2Honor then 
            gs 
            |> changeActivePlayerState (addHonor 2)
        else 
            gs
            |> changeActivePlayerState (addHonor 1)
            |> changeOtherPlayerState (addHonor -1)
        |> next
    gs >!=> choice attacker "Air ring effect" [plus2Honor; take1Honor] effectChosen

let resolveEarthRing attacker next gs =
    gs 
    |> changePlayerState attacker drawCardFromConflictDeck
    |> changePlayerState (otherPlayer attacker) discardRandomConflictCard
    |> next

let resolveVoidRing attacker next gs =
    let remove1fate card (gs:GameState) = 
        gs 
        |> changeCard (fun card -> {card with Fate = card.Fate - 1}) card
        |> next
    gs >!=> chooseCharacter attacker Card.hasFate "Remove 1 fate from character" remove1fate gs
    

let resolveWaterRing attacker next gs =
    let bow = "Bow character without fate"
    let ready = "Ready character"
    let bowChar char gs = gs |> changeCard Card.bow char |> next
    let readyChar char gs = gs |> changeCard Card.ready char |> next
    let effectChosen effect gs =
        if effect = bow then
            gs >!=> chooseCharacter attacker (Card.hasFate >> not) "Bow character" bowChar gs
        else 
            gs >!=> chooseCharacterInPlay attacker "Ready character" readyChar gs
    gs >!=> choice attacker "Water ring effect" [bow; ready] effectChosen 