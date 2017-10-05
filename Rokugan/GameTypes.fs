namespace GameTypes

open RokuganShared

type Clan = 
    | Lion
    | Crane
    | Crab
    | Phoenix
    | Scorpion
    | Unicorn
    | Dragon
    | Neutral

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

type Deck = 
    Deck of Card list
    with
        member this.Cards = 
            let (Deck lst) = this
            lst
        static member Empty = Deck []

type PlayerFlagEnum = Passed

//defines when the flag or trigger is cleared
type Lifetime = Round | Phase | Game | Once


type Command = 
    | ChangePhase of GamePhase
    | RemoveCardState of CardState * Card
    | AddFate of Player * int
    | DynastyPass of Player
    | SwitchActivePlayer 
    | PlayDynasty of Card 
    | AddFateOnCard of Card * int
    | DrawDynastyCard of Player * int  // Card position
    | Bid of Player * int
    | CleanBids
    | AddHonor of Player * int
    | DrawConflictCard of Player * int  // player, number of cards
    | PassConflict of Player
    | DeclareConflict of Player * ConflictType * Ring * Card // attacker, (military/political), ring, province
    | RevealProvince of Card
    | DeclareAttacker of Card
    | DeclareDefender of Card
    | ConflictStarted
    | CollectFateFromRing of Player * Ring
    | DiscardRandomConflict of Player
    | Bow of Card
    | Ready of Card
    | Honor of Card
    | Dishonor of Card
    | BreakProvince of Card
    | DiscardFromPlay of Card
    | AddFateOnRing of Ring * int 
    | ContestRing of Ring
    | ClaimRing of Player * Ring
    | ReturnRing of Ring
    | ApplyBids
    | NextRound

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
    CardsInPlay : Map<int, Card>
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
        member this.Conflict = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = ZoneName.Conflict) |> this.ToValuesList
        member this.Stronghold = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = ZoneName.Stronghold then Some c else None)
        member this.StrongholdProvince = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = ZoneName.StrongholdProvince then Some c else None)
        member this.Provinces = 
            this.CardsInPlay 
            |> Map.filter (fun _ c -> match c.Zone with | ZoneName.Province n -> true | _ -> false)
            |> this.ToValuesList
        member this.CardsInPlayList : Card list = this.CardsInPlay |> Map.toList |> List.map (fun (_, c) -> c)
        static member None = {
            Bid = None
            ConflictDeck = Deck.Empty
            DynastyDeck = Deck.Empty
            Honor = 0
            Fate = 0
            Flags = []
            CardsInPlay = Map.empty
            DeclaredConflicts = [] }

type GameState = 
  { TurnNumber : int
    FirstPlayer : Player
    Player1State : PlayerState
    Player2State : PlayerState 
    Rings : Ring list
    GamePhase : GamePhase
    ActivePlayer : Player
    Triggers : GameTrigger list
    AttackState : AttackState option }
    with
        member this.ActivePlayerState = 
            match this.ActivePlayer with 
            | Player1 -> this.Player1State
            | Player2 -> this.Player2State  
        member this.OtherPlayerState = 
            match this.ActivePlayer with 
            | Player1 -> this.Player2State
            | Player2 -> this.Player1State              
        member this.Cards = 
            List.append this.Player1State.CardsInPlayList this.Player2State.CardsInPlayList
        member this.OtherPlayer =
            match this.ActivePlayer with 
            | Player1 -> Player2
            | Player2 -> Player1

        static member None = {
            TurnNumber = 0
            FirstPlayer =  Player1
            Player1State = PlayerState.None
            Player2State = PlayerState.None 
            Rings = []
            GamePhase = GamePhase.Dynasty
            ActivePlayer = Player1
            Triggers = []
            AttackState = None }
         
and 
    [<StructuredFormatDisplayAttribute("Action ({Player}): {Type}")>]
    PlayerAction = 
      { Type : PlayerActionType
        Player : Player
        OnExecute : Transform }
and Transform = 
  { Commands : Command list 
    NextActions : (GameState -> PlayerAction list) option
    Continuation : (Unit -> Transform) list}

and
    [<StructuredFormatDisplayAttribute("Trigger: {Name}")>]
    GameTrigger = 
      { Name : string
        //Lifetime : Lifetime
        Condition : Command -> GameState -> bool
        Transform : Transform}

type GameStateMod = GameState -> GameState


type GameModel =
  { State: GameState
    Actions : PlayerAction list 
    Continuations : (Unit -> Transform) list
    Log : Command list }

type InitialPlayerConfig = {
    ConflictDeck : CardTitle list
    DynastyDeck : CardTitle list
    Stonghold : CardTitle
    StrongholdProvince : CardTitle
    Provinces : CardTitle list }    