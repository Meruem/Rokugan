module Card

open GameTypes
open CardRepository

let (|Dynasty|_|) card =
    match (repository.GetCard card.Title).Spec with
    | CardSpec.Dynasty _ -> Some card
    | _ -> None

let (|Conflict|_|) card =
    match (repository.GetCard card.Title).Spec with
    | CardSpec.Conflict _ -> Some card
    | _ -> None

let (|Character|_|) card = 
    match repository.GetCard card.Title with
    | CardDef.Character c -> Some card
    | _ -> None 

// ----------- Card creation ------------
let createCard title player zone =
  { Id = CardId (Utils.newId ())
    Title = title
    Owner = player
    States = Set.empty
    Fate = 0 
    Zone = zone}

let createProviceCard title player nr = createCard title player (Province nr)
let createStrongholdProvinceCard title player = createCard title player StrongholdProvince
let createStrongholdCard title player = createCard title player Stronghold

// ----------- State queries --------------

let hasState state card = card.States |> Set.contains state

let isProvinceBroken = hasState Broken
let hasFate (card:Card) = card.Fate > 0
let isBowed = hasState Bowed
let isReady = isBowed >> not
let isHidden = hasState Hidden

let charSkillValue cType card =
    let cardDef = repository.GetCard card.Title
    match  cardDef with
    | CardDef.Character char ->  
        match cType with
        | Military -> char.MilitarySkill
        | Political ->char.PoliticalSkill
    | _ -> None 

let isCharWithValue cType card = 
    charSkillValue cType card |> Option.isSome 

let isCharacter = function | Character _ -> true | _ -> false

let dynastyCardPosition card =
    match card.Zone with
    | DynastyInProvinces n -> n
    | _ -> failwith "Card is not a dynasty card in provice"

// ----------- State changes --------------

let removeCardState state card = {card with States = card.States |> Set.remove state }

let addCardState state card = {card with States = card.States |> Set.add state }

let revealProvince = removeCardState CardState.Hidden

let honor card = 
    if hasState Dishonored card then removeCardState Dishonored card
    else if not (hasState Honored card) then addCardState Honored card
        else card

let dishonor card = 
    if hasState Honored card then removeCardState Honored card
    else if not (hasState Dishonored card) then addCardState Dishonored card
        else card

let discardConflict card = {card with Zone = ConflictDiscard}
let discardDynasty card = {card with Zone = DynastyDiscard}

let discard card = 
    match card with
    | Dynasty d -> discardDynasty d
    | Conflict c -> discardConflict c
    | _ -> card

let bow = addCardState Bowed
let ready = removeCardState Bowed
let breakProvince = addCardState Broken
let putAdditionalFate fate (card:Card) = {card with Fate = fate}
let move zone card = {card with Zone = zone}

let addFateOnCard fate (card:Card) = {card with Fate = max (card.Fate + fate) 0}
  