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

type ProvinceState = Hidden | Revealed | Broken

type CardState = Bowed | Honored | Dishonored | Hidden

type Card = {
    Title : CardTitle
    Owner : Player
    States : CardState list
    Fate : int }

type Province = {
    ProvinceCard : Card
    State : ProvinceState }

type Stronghold = { StrongholdCard : Card }

type Deck = 
    Deck of Card list
        member this.Cards = 
            let (Deck lst) = this
            lst
type Zone = 
    Zone of Card list
        member this.Cards = 
            let (Zone lst) = this
            lst

type PlayerFlagEnum = Passed

//defines when the flag is cleared
type Lifetime = Round | Phase | Game

type PlayerFlag =
  { Lifetime : Lifetime
    Flag : PlayerFlagEnum}

type PlayerState = {
    ConflictDeck : Deck
    DynastyDeck : Deck
    DynastyDiscard : Zone
    ConflictDiscard : Zone 
    Honor : int
    Fate : int
    Hand : Zone
    DynastyInProvinces : Zone
    Stonghold : Stronghold
    StrongholdProvince : Province
    Provinces : Province list
    Home: Zone
    Flags : PlayerFlag list }

type GameEnd = Player1Won | Player2Won 
type GamePhase = Dynasty | Draw | Conflict | Fate | Regroup | End of GameEnd 

type GameState = {
    TurnNumber : int
    Actions : PlayerAction list
    GamePhase : GamePhase
    ActivePlayer : Player
    FirstPlayer : Player
    Triggers : GameTrigger list
    Player1State : PlayerState
    Player2State : PlayerState }
and GameTrigger = 
  { Name : string
    Lifetime : Lifetime
    Condition : GameState -> bool
    Action : GameState -> GameState}
and PlayerActionType = 
    | Pass
    | PlayCharacter of CardTitle
    | ActivateAction
    | Choice of int
and PlayerAction = 
  { Type : PlayerActionType
    Action : GameState -> GameState }

type InitialPlayerConfig = {
    Player : Player
    ConflictDeck : CardTitle list
    DynastyDeck : CardTitle list
    Stonghold : CardTitle
    StrongholdProvince : CardTitle
    Provinces : CardTitle list }    