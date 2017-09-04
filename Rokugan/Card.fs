module Card

open GameTypes

let createCard title player =
  { Id = CardId (Utils.newId ())
    Title = title
    Owner = player
    States = []
    Fate = 0 }

let createProviceCard title player = 
  { ProvinceCard = createCard title player
    State = ProvinceState.Hidden }

let createStrongholdCard title player = { StrongholdCard = createCard title player }

let removeCardState state (card:Card) =
     { card with States = card.States |> List.filter (fun s -> s <> state) }

let hasState state card = card.States |> List.contains state

let isBowed = hasState Bowed

let isCharWithValue cType card = 
    let cardDef = CardRepository.getCard card.Title
    match CardDef.character cardDef with
    | None -> false 
    | Some char -> 
        match cType with
        | Military -> Option.isSome char.MilitarySkill
        | Political -> Option.isSome char.PoliticalSkill

let revealProvince province = { province with Province.State = Revealed}

let charSkillValue cType card =
    let cardDef = CardRepository.getCard card.Title
    match CardDef.character cardDef with
    | None -> None 
    | Some char -> 
        match cType with
        | Military -> char.MilitarySkill
        | Political ->char.PoliticalSkill
