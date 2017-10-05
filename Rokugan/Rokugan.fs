module Rokugan.Progrem

open GameTypes
open SampleDeck
open CardDef
open GameState
open Game
open CardRepository
open System

let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

[<EntryPoint>]
let main argv =
    do CardRepository.repository.AddCards CoreCards.coreCards 
    let p1config,p2config = samplePlayerConfigs ()


    let mutable gs = 
        startGame p1config p2config (GameUtils.chooseRandomPlayer ())

    let rec readlines () = 
        printfn "%A" gs
        let line = Console.ReadLine()
        match line with 
        | Int i -> gs <- (gs |> playAction i)
        | _ -> ()

        if line <> "" then readlines () else false
    readlines () |> ignore
    0
