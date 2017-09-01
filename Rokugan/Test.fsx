#I __SOURCE_DIRECTORY__
#load @"c:\projects\FsEye\FsEye.fsx"
#load @"GameTypes.fs"
#load @"CardDef.fs"
#load @"SampleDeck.fs"
#load @"CoreCards.fs"
#load @"CardRepository.fs"
#load @"GameState.fs"


open GameTypes
open SampleDeck
open CardDef
open CoreCards
open GameState

let randomConflictDeck () = sampleConflictDeck 45 coreCards
let randomDynastyDeck () = sampleDynastyDeck 45 coreCards
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
    |> revealAllDynastyCardsAtProvinces
    |> collectFateFromStronghold
    |> getDynastyPhaseActions