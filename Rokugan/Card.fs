module Card

open GameTypes

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

let removeCardState state (card:Card) =
     { card with States = card.States |> Set.remove state }

let addCardState state card = {card with States = card.States |> Set.add state }

let hasState state card = card.States |> Set.contains state

let isBowed = hasState Bowed
let isReady = isBowed >> not

let isHidden = hasState Hidden

let character card = 
    let cardDef = CardRepository.getCard card.Title
    CardDef.character cardDef 

let isCharWithValue cType card = 
    let cardDef = CardRepository.getCard card.Title
    match CardDef.character cardDef with
    | None -> false 
    | Some char -> 
        match cType with
        | Military -> Option.isSome char.MilitarySkill
        | Political -> Option.isSome char.PoliticalSkill

let revealProvince = addCardState CardState.Revealed

let charSkillValue cType card =
    let cardDef = CardRepository.getCard card.Title
    match CardDef.character cardDef with
    | None -> None 
    | Some char -> 
        match cType with
        | Military -> char.MilitarySkill
        | Political ->char.PoliticalSkill

let honorCard card = 
    if hasState Dishonored card then removeCardState Dishonored card
    else if not (hasState Honored card) then addCardState Honored card
        else card

let dishonorCard card = 
    if hasState Honored card then removeCardState Honored card
    else if not (hasState Dishonored card) then addCardState Dishonored card
        else card

let putAddtitionalFate fate (card:Card) = {card with Fate = fate}


let dynastyCardPosition card =
    match card.Zone with
    | DynastyInProvinces n -> n
    | _ -> failwith "Card is not a dynasty card in provice"

let isProvinceBroken = hasState Broken 

let discardConflict card = {card with Zone = ConflictDiscard}

let hasFate (card:Card) = card.Fate > 0

let bow = addCardState Bowed
let ready = removeCardState Bowed
let breakProvince = addCardState Broken
 