module Tests

open System

open RokuganShared
open GameTypes
open CardRepository
open TestUtils
open Game

open Expecto

do repository.AddCards TestCards.testCards  // do this for selecting card sets 

let allCards = repository.AllCards ()
let gs = 
    startGameTest allCards Player1

[<Tests>]
let tests =
  testList "main" [
    testCase "Bid test" <| fun _ ->
        let gs2 = 
            gs
            |> playAction 0 // pass
            |> playAction 0 // pass
            |> playAction 1 // bid 2 for pl1
            |> playAction 4 // bid 5 for pl2

        Expect.equal (Some 2) gs2.State.Player1State.Bid ""
        Expect.equal (Some 5) gs2.State.Player2State.Bid ""
        Expect.equal 13 gs2.State.Player1State.Honor ""
        Expect.equal 7 gs2.State.Player2State.Honor ""
        Expect.equal 6 gs2.State.Player1State.Hand.Length ""
        Expect.equal 9 gs2.State.Player2State.Hand.Length ""

    testCase "Play dynasty card" <| fun _ ->
        let cards = 
          [ TestCards.``Generic 1/1``
            TestCards.``Province str 4``
            TestCards.Stronghold
            TestCards.Event]
        let gm = 
            startGameTest cards Player1
        let gs2 = gm |> playAction 1 |> playAction 1  
        Expect.equal 1 gs2.State.Player1State.Home.Length ""
        Expect.equal 4 gs2.State.Player1State.DynastyInProvinces.Length ""

    testCase "Dynasty cards should be revealed at beginning of dynasty phase" <| fun _ ->
        let cards = 
          [ TestCards.``Generic 1/1``
            TestCards.``Province str 4``
            TestCards.Stronghold
            TestCards.Event]
        let gm = 
            startGameTest cards Player1
        let card = gm.State.Player1State.DynastyInProvinces.[0]
        let gs' = gm.State |> GameState.changeCard (fun c -> {c with States = c.States |> Set.add Hidden}) card    
        Expect.isTrue (gs'.Player1State.DynastyInProvinces.[0].States |> Set.contains Hidden)  "card is not hidden"
        let cmds = gs' |> Dynasty.revealAllDynastyCardsAtProvinces
        let x = cmds |> List.contains (RemoveCardState (Hidden,card))
        Expect.exists cmds (fun cmd -> match cmd with | RemoveCardState (st, c) when st = Hidden -> c.Id = card.Id | _ -> false)  "reveal command was not created"]


[<EntryPoint>]
let main args =
    printfn "Starts after enter ..."
    Console.ReadLine () |> ignore
    runTestsWithArgs defaultConfig args tests