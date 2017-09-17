#I __SOURCE_DIRECTORY__
#I ".."
#I "../CardSets"

#load @"GameTypes.fs"
#load @"fsiHelpers.fsx"
#load @"CardRepositoryClass.fs"
#load @"Utils.fs"
#load @"CardDef.fs"
#load @"SampleDeck.fs"
#load @"CoreCards.fs"
#load @"CardRepository.fs"
#load @"TestCards.fs"
#load @"Card.fs"
#load @"Deck.fs"
#load @"Command.fs"
#load @"PlayerState.fs"
#load @"GameState.fs"
#load @"Actions.fs"
#load @"Triggers.fs"
#load @"Ring.fs"
#load @"Dynasty.fs"
#load @"Draw.fs"
#load @"Conflict.fs"
#load @"Fate.fs"
#load @"Regroup.fs"
#load @"Game.fs"
#load @"TestUtils.fs"


open GameTypes
open SampleDeck
open CardDef
open GameState
open Game
open CardRepository
open TestUtils

do repository.AddCards TestCards.testCards  // do this for selecting card sets 
//#load "GameRepository.fs" // by simply loading this, all core cards are added

let allCards = repository.AllCards ()
let gs = 
    startGameTest allCards (Utils.chooseRandomPlayer ())


let gs2 = 
    gs
    |> playAction 0 // pass
    |> playAction 0 // pass
    |> playAction 1 // bid 2 for pl1
    |> playAction 4 // bid 5 for pl2

let testBiding = 
    gs2.State.Player1State.Bid = Some 2 
    && gs2.State.Player2State.Bid = Some 5 
    && gs2.State.Player1State.Honor = 13
    && gs2.State.Player2State.Honor = 7
    && gs2.State.Player1State.Hand.Length = 6
    && gs2.State.Player2State.Hand.Length = 9

// let gs3 = 
//     gs
//     |> playAction 1 // 1st char
//     |> playAction 1 // add 1 fate
//     |> playAction 1 // 1st char
//     |> playAction 1 // add 1 fategs
//     |> playAction 1 // 2nd char
//     |> playAction 0 // no fate
//     |> playAction 0 // pass
//     |> playAction 0 // pass
//     |> playAction 1 // bid 2
//     |> playAction 1 // bid 2
//     |> playAction 1 // conflict

let mutable gsa = gs

gsa <- gsa |> playAction 0; gsa;;


let map = Map.empty |> Map.add 1 "neco" |> Map.add 1 "nic"
let i = map.Item 1