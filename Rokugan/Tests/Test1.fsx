#I __SOURCE_DIRECTORY__
#I ".."

#load @"GameTypes.fs"
#load @"Utils.fs"
#load @"CardDef.fs"
#load @"SampleDeck.fs"
#load @"CoreCards.fs"
#load @"CardRepository.fs"
#load @"Card.fs"
#load @"Deck.fs"
#load @"PlayerState.fs"
#load @"GameState.fs"
#load @"Actions.fs"
#load @"Triggers.fs"
#load @"Dynasty.fs"
#load @"Draw.fs"
#load @"Conflict.fs"
#load @"Fate.fs"
#load @"Regroup.fs"
#load @"Game.fs"



open GameTypes
open SampleDeck
open CardDef
open CoreCards
open GameState
open Game

let randomConflictDeck () = sampleConflictDeck 10 coreCards
let randomDynastyDeck () = sampleDynastyDeck 10 coreCards
let main :: provinces = sampleProvinces 5 coreCards
let stronghold () = sampleStronghold coreCards

let p1config = 
    {
        Player = Player1
        ConflictDeck = randomConflictDeck ()
        DynastyDeck = randomDynastyDeck ()
        Stonghold = Title "Golden Plains Outpost"
        StrongholdProvince = main
        Provinces = provinces
    }
let p2config = 
    {
        Player = Player2
        ConflictDeck = randomConflictDeck ()
        DynastyDeck = randomDynastyDeck ()
        Stonghold = Title "Golden Plains Outpost"
        StrongholdProvince = main
        Provinces = provinces
    }    

let gs = 
    startGame p1config p2config


let gs2 = 
    gs
    |> playAction 0 // pass
    |> playAction 0 // pass
    |> playAction 1 // bid 2 for pl1
    |> playAction 4 // bid 5 for pl2

let testBiding = 
    gs2.Player1State.Bid = Some 2 
    && gs2.Player2State.Bid = Some 5 
    && gs2.Player1State.Honor = 13
    && gs2.Player2State.Honor = 7
    && gs2.Player1State.Hand.Length = 6
    && gs2.Player2State.Hand.Length = 9

let gs3 = 
    gs
    |> playAction 1 // 1st char
    |> playAction 1 // add 1 fate
    |> playAction 1 // 1st char
    |> playAction 1 // add 1 fate
    |> playAction 1 // 2nd char
    |> playAction 0 // no fate
    |> playAction 0 // pass
    |> playAction 0 // pass
    |> playAction 1 // bid 2
    |> playAction 1 // bid 2
    |> playAction 1 // conflict

let mutable gsa = gs3

gsa <- gsa |> playAction 0; gsa;;

