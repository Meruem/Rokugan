open System

open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open Suave.Json
open Suave.Writers

open System.Threading
open System.Runtime.Serialization
open System.Runtime.Serialization.Json
open System.IO
open System.Text

open SampleDeck
open Game

let toJson<'t> (myObj:'t) =
    use ms = new MemoryStream() 
    (DataContractJsonSerializer(typeof<'t>)).WriteObject(ms, myObj) 
    Encoding.Default.GetString(ms.ToArray()) 

do CardRepository.repository.AddCards CoreCards.coreCards 
let p1config,p2config = samplePlayerConfigs ()

let mutable gs = 
    startGame p1config p2config (Utils.chooseRandomPlayer ())

let webApp = 
    path "/gamestate" 
        >=> OK (toJson gs) 
        >=> setMimeType "application/json; charset=utf-8"

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync conf (OK "FFF") //webApp

    Async.Start(server, cts.Token)
    printfn "Make requests now"
    let x = toJson gs.State
    printfn "%A" x

    Console.ReadKey true |> ignore

    cts.Cancel()
    0