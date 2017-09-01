module CardRepository
open CoreCards
open GameTypes
open System
let allCards = coreCards
let allCardsMap = [for card in allCards do yield card.Title, card] |> Map.ofList
let fail title = failwithf "Unknown card %s" (string title)

let getCard (title : CardTitle) = 
    if allCardsMap.ContainsKey title then allCardsMap.[title] else failwith ("Unknown card " + string title)

let getDynastyCard (title:CardTitle) =
    let card = getCard title
    match card.Spec with 
    | CardSpec.Dynasty d -> d
    | _ -> fail title

let getCharacterCard (title:CardTitle) =
    let card = getCard title
    match card.Spec with 
    | CardSpec.Dynasty d -> 
        match d with 
        | DynastyCardDef.Character c -> c
        | _ -> fail title
    | _ -> fail title

let getStrongholdCard title =
    let card = getCard title  
    match card.Spec with | Stronghold s -> s | _ -> fail title
