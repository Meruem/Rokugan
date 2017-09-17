module TestUtils

open GameTypes
open SampleDeck
open CardRepositoryClass
open Game

let rec zeroAction () = 
  { Type = Test
    Player = Player1
    Commands = []
    NextActions = fun _ -> [zeroAction ()]}
let idTrans = { Commands = []; NextActions = fun _ -> [zeroAction ()]}

let startGameTest allCards startPlayer =
    let randomConflictDeck () = sampleConflictDeck 10 allCards
    let randomDynastyDeck () = sampleDynastyDeck 10 allCards
    let main, provinces = 
        match (sampleProvinces 5 allCards) with
        | x :: xs -> x,  xs
        | [] -> failwith "!"
    let stronghold () = sampleStronghold allCards
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

    startGame p1config p2config startPlayer 