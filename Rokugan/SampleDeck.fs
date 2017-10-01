module SampleDeck

open RokuganShared
open GameTypes
open CardDef

let rnd = System.Random()

let sampleDeck num (defList: List<_>) =
    [1..num] 
        |> List.map (fun _ -> 
            defList.[rnd.Next(defList.Length)])

let getTitle (def:CardDef) = def.Title

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

let samplePlayerConfigs () = 
    let randomConflictDeck () = sampleConflictDeck 10 (CardRepository.repository.AllCards ())
    let randomDynastyDeck () = sampleDynastyDeck 10 (CardRepository.repository.AllCards ())
    let allProvinces = sampleProvinces 5 (CardRepository.repository.AllCards ())
    
    let main, provinces = 
        match allProvinces with
        | x :: xs -> x,xs
        | [] -> failwith "not enaugh provinces defined"
    let stronghold () = sampleStronghold (CardRepository.repository.AllCards ())
    let p1config = 
        {
            ConflictDeck = randomConflictDeck ()
            DynastyDeck = randomDynastyDeck ()
            Stonghold = stronghold ()
            StrongholdProvince = main
            Provinces = provinces
        }
    let p2config = 
        {
            ConflictDeck = randomConflictDeck ()
            DynastyDeck = randomDynastyDeck ()
            Stonghold = stronghold ()
            StrongholdProvince = main
            Provinces = provinces
        }    
    p1config, p2config

