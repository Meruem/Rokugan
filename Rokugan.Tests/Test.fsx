#I __SOURCE_DIRECTORY__
#I ".."
#I "../Rokugan/CardSets"
#I "../packages"
#I "../Rokugan.Shared"
#I "../Rokugan"

//#r "Newtonsoft.Json/lib/netstandard1.3/Newtonsoft.Json.dll"
//#r "Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"

#load @"SharedTypes.fs"
#load @"Utils.fs"
#load @"GameTypes.fs"
#load @"GameUtils.fs"
#load @"fsiHelpers.fsx"
#load @"CardRepositoryClass.fs"
#load @"Transform.fs"
#load @"CardDef.fs"
#load @"CoreCards.fs"
#load @"CardRepository.fs"
#load @"SampleDeck.fs"
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

open RokuganShared
open GameTypes
open SampleDeck
open CardDef
open GameState
open Game
open CardRepository
open TestUtils

do repository.AddCards TestCards.testCards  // do this for selecting card sets 
// do repository.AddCards CoreCards.coreCards // core cards



let allCards = repository.AllCards ()
let gs = 
    startGameTest allCards (GameUtils.chooseRandomPlayer ())

let mutable gsa = gs

let testTrigger = 
    { Name = "test"
      Condition = fun cmd gs -> gs.TurnNumber < 5
      Transform = changes [NextRound]}

//gsa <- gsa |> playAction 0; gsa;;
gsa <- {gsa with State = gsa.State |> Triggers.addTrigger testTrigger }



let addHonorPlayer1 x = changes [AddHonor (Player1,x)]
gsa <- gsa |> update (addHonorPlayer1 -5)


let card = gs.State.Player1State.DynastyInProvinces.[1]

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




let map = Map.empty |> Map.add 1 "neco" |> Map.add 1 "nic"
let i = map.Item 1

// let tst =  Province 3 |> Json.serializeWith zoneNameToJson |> Json.format
// let ds = tst |> Json.parse |> zoneNameFromJson
// let ds2 = 
//     match ds with
//     | Value (z : ZoneName), _ -> z
//     | Error e, _ -> failwith ":("

