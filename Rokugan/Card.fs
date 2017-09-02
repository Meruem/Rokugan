module Card

open GameTypes

let createCard title player =
  { Title = title
    Owner = player
    States = []
    Fate = 0 }

let createProviceCard title player = 
  { ProvinceCard = createCard title player
    State = ProvinceState.Hidden }

let createStrongholdCard title player = { StrongholdCard = createCard title player }

let removeCardState state (card:Card) =
     { card with States = card.States |> List.filter (fun s -> s <> state) }
