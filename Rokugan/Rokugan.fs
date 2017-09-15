module MyWebApi.Program

open Suave
open Suave.Successful
open GameTypes
open SampleDeck
open CardDef
open GameState
open Game
open CardRepository
open GameRepository
open System

let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

[<EntryPoint>]
let main argv =
    do CardRepository.repository.AddCards CoreCards.coreCards 
    let randomConflictDeck () = sampleConflictDeck 10 (repository.AllCards ())
    let randomDynastyDeck () = sampleDynastyDeck 10 (repository.AllCards ())
    let main :: provinces = sampleProvinces 5 (repository.AllCards ())
    let stronghold () = sampleStronghold (repository.AllCards ())
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

    let mutable gs = 
        startGame p1config p2config (Utils.chooseRandomPlayer ())

    let rec readlines () = 
        printfn "%A" gs
        let line = Console.ReadLine()
        match line with 
        | Int i -> gs <- (gs |> playAction i)
        | _ -> ()

        if line <> "" then readlines () else false
    readlines ()
    //startWebServer defaultConfig (OK "Hello World!")
    0
