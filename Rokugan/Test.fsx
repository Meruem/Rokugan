#I __SOURCE_DIRECTORY__
 
//#load @"c:\projects\FsEye\FsEye.fsx"
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
        Stonghold = stronghold ()
        StrongholdProvince = main
        Provinces = provinces
    }
let p2config = 
    {
        Player = Player2
        ConflictDeck = randomConflictDeck ()
        DynastyDeck = randomDynastyDeck ()
        Stonghold = stronghold ()
        StrongholdProvince = main
        Provinces = provinces
    }    

let gs = 
    initializeGameState p1config p2config
    |> startGame

let gs2 = gs |> playAction 0 |> playAction 0