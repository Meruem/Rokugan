open System
open System.Threading

open Suave
open Suave.Successful
open Suave.Filters
open Suave.Operators
open Suave.Json
open Suave.Writers

open Newtonsoft.Json

open SampleDeck
open Game
open RokuganShared
open GameTypes

open Fable.Core.JsInterop
open ClientConvert


do CardRepository.repository.AddCards CoreCards.coreCards 

let p1config,p2config = samplePlayerConfigs ()

let startNewGame () =
    startGame p1config p2config (GameUtils.chooseRandomPlayer ())

let mutable gm = 
    startNewGame()

let getClientModel (gm:GameModel) =
  { State = toClientGameState gm.State
    Log = gm.Log |> List.map (fun c -> c.ToString())
    Actions = toClientPlayerActions gm.Actions }          

let jsonConverter = Fable.JsonConverter() :> JsonConverter
let modelToJson m =
    //toJson m
    JsonConvert.SerializeObject (m, jsonConverter)

let gameState (gm:GameModel) =
    OK (modelToJson (getClientModel gm)) 
    >=> setMimeType "application/json; charset=utf-8"

let webApp = 
    choose
      [ path "/" >=> Files.file "../Rokugan.web.client/public/index.html"
        path "/bundle.js" >=> Files.file "../Rokugan.web.client/public/bundle.js"
        path "/gamestate" >=> (gameState gm)
        path "/newgame" >=> fun c -> 
            gm <- (startNewGame ())
            gameState gm c
        pathScan "/play/%d" (fun d -> 
            gm <- playAction d gm
            (gameState gm))]

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync conf webApp

    Async.Start(server, cts.Token)
    printfn "Make requests now"

    Console.ReadKey true |> ignore

    cts.Cancel()
    0