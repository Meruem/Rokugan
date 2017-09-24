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
open GameTypes

type ClientPlayerAction = { 
    Number : int
    Description : string
    Player : string }

type ClientModel = {
    State : GameState
    Log : Command list
    Actions : ClientPlayerAction list }


do CardRepository.repository.AddCards CoreCards.coreCards 
let p1config,p2config = samplePlayerConfigs ()
let mutable gm = 
    startGame p1config p2config (GameUtils.chooseRandomPlayer ())

let getPlayerActions (gm:GameModel) = 
    gm.Actions
    |> List.mapi (fun i a -> 
        { Number = i
          Description = a.Type.ToString()
          Player = a.Player.ToString()})

let getClientModel (gm:GameModel) =
  { State = gm.State
    Log = gm.Log
    Actions = getPlayerActions gm }          

let gameState (gm:GameModel) =
    OK (JsonConvert.SerializeObject (getClientModel gm)) 
    >=> setMimeType "application/json; charset=utf-8"

let webApp = 
    choose
      [ path "/gamestate" >=> (gameState gm)
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