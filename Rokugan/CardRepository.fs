module CardRepository
open CoreCards
open CardDef
open System
let allCards = coreCards
let allCardsMap = [for card in allCards do yield card.Title, card] |> Map.ofList

let getCard (title : CardTitle) = 
    if allCardsMap.ContainsKey title then allCardsMap.[title] else failwith ("Unknown card " + string title)

let getStrongholdCard title =
    let card = getCard title  
    match card.Spec with | Stronghold s -> s | _ -> failwith ("Unknown card " + string title)
