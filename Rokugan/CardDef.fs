module CardDef

open GameTypes

let dynastyCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | CardSpec.Dynasty d -> yield d | _ -> ()]

let filterConflictCards = List.filter (fun card -> match card.Spec with | CardSpec.Conflict c -> true | _ -> false)
let filterDynastyCards = List.filter (fun card -> match card.Spec with | CardSpec.Dynasty c -> true | _ -> false)
let filterStrongholdCards = List.filter (fun card -> match card.Spec with | CardSpec.Stronghold c -> true | _ -> false)
let filterProvinceCards = List.filter (fun card -> match card.Spec with | CardSpec.Province c -> true | _ -> false)

let conflictCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | CardSpec.Conflict c -> yield c | _ -> ()]
let strongholdCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | CardSpec.Stronghold c -> yield c | _ -> ()]    
let provinceCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | CardSpec.Province c -> yield c | _ -> ()]   

let (|Character|_|) cardDef = 
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
