module Tests

open System
open Xunit

open GameTypes
open CardRepository
open TestUtils
open Game

do repository.AddCards TestCards.testCards  // do this for selecting card sets 
//#load "GameRepository.fs" // by simply loading this, all core cards are added

let allCards = repository.AllCards ()
let gs = 
    startGameTest allCards Player1

[<Fact>]
let ``Bid test`` () =
    let gs2 = 
        gs
        |> playAction 0 // pass
        |> playAction 0 // pass
        |> playAction 1 // bid 2 for pl1
        |> playAction 4 // bid 5 for pl2

    Assert.Equal (Some 2, gs2.State.Player1State.Bid) 
    Assert.Equal (Some 5, gs2.State.Player2State.Bid) 
    Assert.Equal (13, gs2.State.Player1State.Honor) 
    Assert.Equal (7, gs2.State.Player2State.Honor) 
    Assert.Equal (6, gs2.State.Player1State.Hand.Length) 
    Assert.Equal (9, gs2.State.Player2State.Hand.Length) 

[<Fact>]
let ``Play dynasty card`` () =
    let cards = 
      [ TestCards.``Generic 1/1``
        TestCards.``Province str 4``
        TestCards.Stronghold
        TestCards.Event]
    let gs = 
        startGameTest cards Player1
    let gs2 = gs |> playAction 1 |> playAction 1  
    Assert.Equal (1, gs2.State.Player1State.Home.Length)
    
