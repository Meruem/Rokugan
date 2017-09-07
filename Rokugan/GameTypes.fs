namespace GameTypes

type CardTitle = Title of string

type Clan = 
    | Lion
    | Crane
    | Crab
    | Phoenix
    | Scorpion
    | Unicorn
    | Dragon
    | Neutral

type Element = Fire | Water | Air | Earth | Void

type Trait =  Bushi | Shugenja | Water | Weapon | Academy
type CardSet = Core

type Ability = Ability of string 

type CharacterCardDef = {
    Cost : int
    Clan : Clan
    MilitarySkill : int option
    PoliticalSkill : int option
    Glory : int
    Traits : Trait list
    Set : CardSet
    Ability : Ability }

type HoldingCardDef = {
    Clan : Clan
    BonusStrength : int     
    Traits : Trait list   
    Ability : Ability
    Set : CardSet }

type DynastyCardDef = 
    | Character of CharacterCardDef
    | Holding of HoldingCardDef

type EventCardDef = {
    Clan : Clan
    Cost : int
    Ability : Ability }

type AttachmentCardDef = {
    Cost : int
    Clan : Clan
    BonusMilitary : int 
    BonusPolitical : int 
    Traits : Trait list
    Ability : Ability }

type StrongholdCardDef = {
    Clan : Clan
    BonusStrength : int
    StartingHonor : int
    FatePerRound : int
    Influence : int
    Ability : Ability }


type ConflictCardDef = 
    | Character of CharacterCardDef
    | Event of EventCardDef
    | Attachment of AttachmentCardDef

type ProvinceCardDef = {
    Strength : int
    Clan : Clan
    Element : Element
    Ability : Ability }

type RoleCardDef = {
    Ability : Ability
    Traits : Trait list }

type CardSpec = 
    | Dynasty of DynastyCardDef
    | Conflict of ConflictCardDef
    | Stronghold of StrongholdCardDef
    | Province of ProvinceCardDef
    | Role of RoleCardDef

type CardDef = {
    Title : CardTitle
    Spec : CardSpec }

type Player = Player1 | Player2

type CardState = Bowed | Honored | Dishonored | Hidden |Revealed | Broken
type CardId = CardId of int

type ZoneName = 
    | Hand
    | DynastyDiscard 
    | ConflictDiscard 
    | DynastyInProvinces of int
    | Home 
    | Conflict 
    | Province of int
    | Stronghold 
    | StrongholdProvince 
    | DynastyDeck 
    | ConflictDeck

type Card = {
    Id : CardId
    Title : CardTitle
    Owner : Player
    States : CardState Set
    Fate : int 
    Zone : ZoneName }


type Deck = 
    Deck of Card list
        member this.Cards = 
            let (Deck lst) = this
            lst

type PlayerFlagEnum = Passed

//defines when the flag or trigger is cleared
type Lifetime = Round | Phase | Game | Once
type ConflictType = Military | Political

type PlayerFlag =
  { Lifetime : Lifetime
    Flag : PlayerFlagEnum}

type PlayerState = {
    Bid : int option
    ConflictDeck : Deck
    DynastyDeck : Deck
    Honor : int
    Fate : int
    Flags : PlayerFlag list
    CardsInPlay : Map<CardId, Card>
    DeclaredConflicts : ConflictType option list }
    with
        member private this.ToValuesList = Map.toList >> List.map (fun (_,c) -> c)
        member this.Home = 
            this.CardsInPlay 
            |> Map.filter (fun _ c -> c.Zone = Home)
            |> this.ToValuesList
        member this.DynastyDiscard = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = DynastyDiscard) |> this.ToValuesList
        member this.ConflictDiscard = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = ConflictDiscard) |> this.ToValuesList
        member this.Hand = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = Hand) |> this.ToValuesList
        member this.DynastyInProvinces = 
            this.CardsInPlay 
            |> Map.filter (fun _ c -> match c.Zone with | DynastyInProvinces _ -> true | _ -> false )
            |> this.ToValuesList
        member this.Conflict = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = Conflict) |> this.ToValuesList
        member this.Stronghold = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = Stronghold then Some c else None)
        member this.StrongholdProvince = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = StrongholdProvince then Some c else None)
        member this.Provinces = 
            this.CardsInPlay 
            |> Map.filter (fun _ c -> match c.Zone with | Province n -> true | _ -> false)
            |> this.ToValuesList
        member this.CardsInPlayList : Card list = this.CardsInPlay |> Map.toList |> List.map (fun (_, c) -> c)

type GameEnd = Player1Won | Player2Won 
type GamePhase = Dynasty | Draw | Conflict | Fate | Regroup | End of GameEnd 
type RingState = Unclaimed | Contested | Claimed of Player
type YesNo = Yes | No

type Ring = 
  { Element : Element
    State : RingState
    Fate : int }

type GameState = 
  { TurnNumber : int
    FirstPlayer : Player
    Triggers : GameTrigger list
    Player1State : PlayerState
    Player2State : PlayerState 
    Rings : Ring list
    GamePhase : GamePhase
    Actions : PlayerAction list
    ActivePlayer : Player }
    with
        member this.ActivePlayerState = 
            match this.ActivePlayer with 
            | Player1 -> this.Player1State
            | Player2 -> this.Player2State  
        member this.Cards = 
            List.append this.Player1State.CardsInPlayList this.Player2State.CardsInPlayList
and GameTrigger = 
  { Name : string
    Lifetime : Lifetime
    Condition : GameState -> bool
    Action : GameState -> GameState}
and PlayerActionType = 
    | Pass of Player
    | PlayCharacter of CardTitle
    | ActivateAction
    | Choicei of int * string
    | Choice of string * string
    | YesNoChoice of YesNo * string
    | DeclareAttack of ConflictType * Element
    | ChooseAttacker of Card 
    | ChooseDefender of Card
    | ChooseProvince of Card
    | ChooseCharacter of Card * string
    | ChooseCard of Card * string
    | ChooseDynastyToDiscard of Card
and PlayerAction = 
  { Type : PlayerActionType
    Action : GameState -> GameState }

type InitialPlayerConfig = {
    ConflictDeck : CardTitle list
    DynastyDeck : CardTitle list
    Stonghold : CardTitle
    StrongholdProvince : CardTitle
    Provinces : CardTitle list }    