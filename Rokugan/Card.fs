module Card

open RokuganShared
open GameTypes
open CardRepository

let (|Dynasty|_|) (card:Card) =
    match (repository.GetCard card.Title).Spec with
    | CardSpec.Dynasty _ -> Some card
    | _ -> None

let (|Conflict|_|) (card: Card) =
    match (repository.GetCard card.Title).Spec with
    | CardSpec.Conflict _ -> Some card
    | _ -> None

let (|CharacterDef|_|) cardDef = 
    match cardDef.Spec with 
    | CardSpec.Dynasty d ->
        match d with 
        | DynastyCardDef.Character c -> Some c
        | _ -> None
    | CardSpec.Conflict c ->
        match c with
        | ConflictCardDef.Character char -> Some char
        | _ -> None
    | _ -> None    

let (|Character|_|) (card:Card) = 
    match repository.GetCard card.Title with
    | CharacterDef c -> Some card
    | _ -> None 
    

// ----------- Card creation ------------
let createCard title player zone =
  { Id = (Utils.newId ())
    Title = title
    Owner = player
    States = Set.empty
    Fate = 0 
    Zone = zone}

let createProviceCard title player nr = createCard title player (ZoneName.Province nr)
let createStrongholdProvinceCard title player = createCard title player StrongholdProvince
let createStrongholdCard title player = createCard title player ZoneName.Stronghold

// ----------- State queries --------------

let hasState state card = card.States |> Set.contains state

let isProvinceBroken = hasState Broken
let hasFate (card:Card) = card.Fate > 0
let isBowed = hasState Bowed
let isReady = isBowed >> not
let isHidden = hasState Hidden

let charSkillValue cType (card:Card) =
    let cardDef = repository.GetCard card.Title
    match  cardDef with
    | CharacterDef char ->  
        match cType with
        | Military -> char.MilitarySkill
        | Political ->char.PoliticalSkill
    | _ -> None 

let isCharWithValue cType (card:Card) = 
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

let discard (card:Card) = 
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
  
