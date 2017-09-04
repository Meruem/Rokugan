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
#load @"Triggers.fs"
#load @"Dynasty.fs"
#load @"Draw.fs"
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
    initializeGameState p1config p2config
    |> startGame
    |> playAction 0 // pass
    |> playAction 0 // pass
    |> playAction 1 // bid 2 for pl1
    |> playAction 4 // bid 5 for pl2

let testBiding = 
    gs.Player1State.Bid = Some 2 
    && gs.Player2State.Bid = Some 5 
    && gs.Player1State.Honor = 13
    && gs.Player2State.Honor = 7
    && gs.Player1State.Hand.Cards.Length = 6
    && gs.Player2State.Hand.Cards.Length = 9
