module GameState

open CardDef
open CardRepository
open CoreCards

type Player = Player1 | Player2

type ProvinceState = Hidden | Revealed | Broken

type CardState = Bowed | Honored | Dishonored

type Card = 
    {
        Title : CardTitle
        Owner : Player
        States : CardState list
    }

type Province =
    {
        ProvinceCard : Card
        State : ProvinceState
    }

type Stronghold = 
    {
        StrongholdCard : Card
    }

type PlayerState = 
    {
        Honor : int
        Fate : int
        ConflictDeck : Card list
        DynastyDeck : Card list
        Hand : Card list
        DynastyInProvinces : Card list
        Stonghold : Stronghold
        StrongholdProvince : Province
        Provinces : Province list
    }

type Gamestate =
    {
        FirstPlayer : Player
        Player1State : PlayerState
        Player2State : PlayerState
    }

type InitialPlayerConfig =
    {
        Player : Player
        ConflictDeck : CardTitle list
        DynastyDeck : CardTitle list
        Stonghold : CardTitle
        StrongholdProvince : CardTitle
        Provinces : CardTitle list
    }

let chooseRandomPlayer () = 
    let rnd = System.Random()
    if rnd.Next(2) = 0 then Player1 else Player2

let drawCardsFromDeck n (deck : Card list) = 
    let c = min n deck.Length
    List.splitAt c deck

let initializeCard title player =
    {
        Title = title
        Owner = player
        States = []
    }

let initializeProvice title player = 
    {
        ProvinceCard = initializeCard title player
        State = Hidden
    }

let initializeStronghold title player = 
    {
        StrongholdCard = initializeCard title player
    }

let initializePlayerState (initialConfig:InitialPlayerConfig) =
    let init card = initializeCard card initialConfig.Player 
    let conflictDeck = initialConfig.ConflictDeck |> List.map init
    let hand, conflictDeck' = drawCardsFromDeck 4 conflictDeck
    let dynastyDeck = initialConfig.DynastyDeck |> List.map init
    let dynastyHand, dynastyDeck' = drawCardsFromDeck 4 dynastyDeck
    let initProvince title = initializeProvice title initialConfig.Player
    let stronghold = getStrongholdCard initialConfig.Stonghold
    {
        Honor = stronghold.StartingHonor
        Fate = 0
        ConflictDeck = conflictDeck'
        DynastyDeck = dynastyDeck'
        Hand = hand
        DynastyInProvinces = dynastyHand
        Stonghold = initializeStronghold initialConfig.Stonghold initialConfig.Player
        StrongholdProvince =  initProvince initialConfig.StrongholdProvince
        Provinces = initialConfig.Provinces |> List.map  initProvince
    } 

let initializeGameState playerConfig1 playerConfig2 =
    {
        FirstPlayer = chooseRandomPlayer ()
        Player1State = initializePlayerState playerConfig1
        Player2State = initializePlayerState playerConfig2       
    }
