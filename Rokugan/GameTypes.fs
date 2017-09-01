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
    States : CardState list }

type Province = {
    ProvinceCard : Card
    State : ProvinceState }

type Stronghold = { StrongholdCard : Card }

type PlayerState = {
    Honor : int
    Fate : int
    ConflictDeck : Card list
    DynastyDeck : Card list
    Hand : Card list
    DynastyInProvinces : Card list
    Stonghold : Stronghold
    StrongholdProvince : Province
    Provinces : Province list
    Home: Card list
    DynastyDiscard : Card list
    ConflictDiscard : Card list }

type GamePhase = Dynasty | Draw | Conflict | Fate | Regroup

type GameState = {
    AvailablePlayerActions : PlayerAction list
    GamePhase : GamePhase
    ActivePlayer : Player
    FirstPlayer : Player
    Player1State : PlayerState
    Player2State : PlayerState }
and GameStateChanger = GameState -> GameState
and PlayerAction = 
    | Pass of GameStateChanger
    | PlayCharacter of CardTitle * GameStateChanger
    | ActivateAction of GameStateChanger

type InitialPlayerConfig = {
    Player : Player
    ConflictDeck : CardTitle list
    DynastyDeck : CardTitle list
    Stonghold : CardTitle
    StrongholdProvince : CardTitle
    Provinces : CardTitle list }    