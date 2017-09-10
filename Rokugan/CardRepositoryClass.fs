namespace CardRepositoryClass

open GameTypes

type Repository() = 
    let allCardsDict = new System.Collections.Generic.Dictionary<CardTitle,CardDef>()
    let addCards (newcards:CardDef list) = [for card in newcards do yield card.Title, card] |> List.iter allCardsDict.Add
    let fail title = failwithf "Unknown card %s" (string title)

    member this.AllCards () = allCardsDict |> Seq.map (fun (KeyValue (k,v)) -> v) |> List.ofSeq
    member this.AddCards(cards) = 
        addCards cards
    member this.GetCard (title : CardTitle) = 
        if allCardsDict.ContainsKey title then allCardsDict.[title] else failwith ("Unknown card " + string title)

    member this.GetDynastyCard (title:CardTitle) =
        let card = this.GetCard title
        match card.Spec with 
        | CardSpec.Dynasty d -> d
        | _ -> fail title

    member this.GetProvinceCard (title:CardTitle) =
        let card = this.GetCard title
        match card.Spec with 
        | CardSpec.Province d -> d
        | _ -> fail title

    member this.GetCharacterCard (title:CardTitle) =
        let card = this.GetCard title
        match card.Spec with 
        | CardSpec.Dynasty d -> 
            match d with 
            | DynastyCardDef.Character c -> c
            | _ -> fail title
        | _ -> fail title

    member this.GetStrongholdCard title =
        let card = this.GetCard title  
        match card.Spec with | CardSpec.Stronghold s -> s | _ -> fail title

