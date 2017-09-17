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

let isUnclaimed (ring:Ring) = ring.State = Unclaimed

let resolveFireRing attacker next =
    let honor = "Honor character"
    let dishonor = "Dishonor character"
    let honorChar char = 
        [Honor char] @+ next
    let dishonorChar char =
        [Dishonor char] @+ next
    let effectChosen effect =
        if effect = honor then
            { Commands = []
              NextActions = chooseCharacterInPlay attacker "Honor character" honorChar }
        else 
            { Commands = []
              NextActions = chooseCharacterInPlay attacker "Dishonor character" dishonorChar }
    { Commands = []
      NextActions =  fun _ -> choice attacker "Fire ring effect" [honor; dishonor] effectChosen }

let resolveAirRing attacker cont = 
    let plus2Honor = "+2 Honor"
    let take1Honor = "Take 1 honor"
    let effectChosen effect = 
        if effect = plus2Honor then 
            [AddHonor (attacker, 2)]
        else 
            [AddHonor (attacker, 1)
             AddHonor (otherPlayer attacker, -1)]
        @+ cont
    { Commands = []    
      NextActions = fun _ -> choice attacker "Air ring effect" [plus2Honor; take1Honor] effectChosen }

let resolveEarthRing attacker next =
    { Commands = 
        [DrawConflictCard (attacker, 1)
         DiscardRandomConflict (otherPlayer attacker)]
        @ next.Commands 
      NextActions = next.NextActions}

let resolveVoidRing attacker cont =
    let remove1fate card = 
        { Commands = [AddFateOnCard (card, -1)] @ cont.Commands
          NextActions = cont.NextActions }
    { Commands = []
      NextActions = chooseCharacter attacker Card.hasFate "Remove 1 fate from character" remove1fate }

let resolveWaterRing attacker next =
    let bow = "Bow character without fate"
    let ready = "Ready character"
    let bowChar char = 
        [Bow char] @+ next
    let readyChar char =
        [Ready char] @+ next
    let effectChosen effect =
        if effect = bow then
            { Commands = []
              NextActions = chooseCharacter attacker (Card.hasFate >> not) "Bow character" bowChar }
        else 
            { Commands = []
              NextActions = chooseCharacterInPlay attacker "Ready character" readyChar }
    { Commands = []
      NextActions =  fun _ -> choice attacker "Water ring effect" [bow; ready] effectChosen }

let addFate amount (ring:Ring) = {ring with Fate = ring.Fate + amount}

let onAddFateOnRing ring amount = changeRing ring.Element (addFate amount)
let onClaimRing = claimRing
let onContestRing = contestRing
let onReturnRing = returnRing