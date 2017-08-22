module SampleDeck

open CardDef

let rnd = System.Random()

let sampleDeck num (defList: List<_>) =
    [1..num] 
        |> List.map (fun _ -> 
            defList.[rnd.Next(defList.Length)])

let getTitle def = def.Title

let sampleConflictDeck num (defList : CardDef list) =
    defList 
        |> filterConflictCards
        |> sampleDeck num
        |> List.map getTitle
    
let sampleDynastyDeck num (defList : CardDef list) =
    defList 
        |> filterDynastyCards
        |> sampleDeck num
        |> List.map getTitle

let sampleStronghold (defList : CardDef list) =
    defList
        |> filterStrongholdCards
        |> List.head
        |> getTitle

let sampleProvinces num (defList : CardDef list) =
    defList 
        |> filterProvinceCards
        |> sampleDeck num
        |> List.map getTitle

