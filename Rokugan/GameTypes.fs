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
    | Undefined

type Trait =  Bushi | Shugenja | Water | Weapon | Academy | Fire | Cavalry
type CardSet = Core | Undefined
type AttackOrDefence = Attack | Defence

//defines when the flag or trigger is cleared
type Lifetime = Round | Phase | Game | Once | Conflict | Undefined

type ActionWindowStarts = FirstPlayer | Defender

type EffectType = CannotBlock | CannotAttack
type DeckType = Conflict | Dynasty

type CardId = int
type TriggerId = int
type CardEffectId = int
type CardPosition = int

type Transform<'gs, 'cmd, 'pa> = 
  { Commands : ('gs -> 'cmd list) option 
    NextActions : ('gs -> PlayerAction<'gs, 'cmd, 'pa> list) option  
    ActionPrompt : string
    Continuation : (Unit -> Transform<'gs, 'cmd, 'pa>) list }    

and 
    [<StructuredFormatDisplayAttribute("Action ({Player}): {Type}")>]
    PlayerAction<'gs, 'cmd, 'pa> = 
      { Type : 'pa
        Player : Player
        OnExecute : Transform<'gs, 'cmd, 'pa> }

[<StructuredFormatDisplayAttribute("Trigger: {Name}")>]
type GameTrigger<'gs, 'cmd, 'pa> = 
      { Id : TriggerId
        Name : string
        Lifetime : Lifetime
        Condition : 'cmd -> 'gs -> bool
        Transform : Transform<'gs, 'cmd, 'pa>}

type GameModel<'gs, 'cmd, 'pa> =
  { State: 'gs
    Actions : PlayerAction<'gs, 'cmd, 'pa> list 
    Triggers : GameTrigger<'gs, 'cmd, 'pa> list
    Continuations : (Unit -> Transform<'gs, 'cmd, 'pa>) list
    Prompt : string
    Log : 'cmd list }

type Deck = 
    Deck of Card list
    with
        member this.Cards = 
            let (Deck lst) = this
            lst
        static member Empty = Deck []

type PlayerFlagEnum = Passed

type ConflictResolution = 
  { Winners : Card list
    Loosers : Card list}

type Command<'gs, 'pa> = 
    | ChangePhase of GamePhase
    | RemoveCardState of CardState * Card
    | AddFate of Player * int
    | DynastyPass of Player
    | SwitchActivePlayer 
    | PlayDynasty of Card * CardPosition 
    | AddFateOnCard of Card * int
    | DrawDynastyCard of Player * CardPosition  // Card position
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
    | EndGame of GameEnd
    | ActionPass of Player
    | CleanPassFlags
    | SetActivePlayer of Player
    | SetFirstPlayerActive
    | Debug of string
    | AddCardEffect of Card * Lifetime * EffectType
    | RemoveCardEffect of CardEffectId
    | ConflictEnd of ConflictResolution option
    | AddTrigger of GameTrigger<'gs, Command<'gs,'pa> , 'pa>
    | RemoveTrigger of TriggerId
    | RevealCard of Card
    | CleanRevealedCards
    | Shuffle of Player * DeckType
    | PutCardFromDeckToHand of CardId

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
            //|> List.sortBy (fun c -> match c.Zone with | DynastyInProvinces n -> n | _ -> -1)
        member this.Conflict = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = ZoneName.Conflict) |> this.ToValuesList
        member this.Stronghold = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = ZoneName.Stronghold then Some c else None)
        member this.StrongholdProvince = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = ZoneName.StrongholdProvince then Some c else None)
        member this.Provinces = 
            this.CardsInPlay 
            |> Map.filter (fun _ c -> match c.Zone with | ZoneName.Province n -> true | _ -> false)
            |> this.ToValuesList
            //|> List.sortBy (fun c -> match c.Zone with | ZoneName.Province n -> n | _ -> -1)            
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
    CardActions : CardAction list
    CardEffects : CardEffect list
    AttackState : AttackState option
    RevealedCards : Card list }
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
            CardEffects = []
            CardActions = []
            AttackState = None
            RevealedCards = [] }

and CardAction = {
    Spec : CardActionDef
    Card : Card
    LastUsed : (GamePhase * int) option }         

and CardActionDef = {
    Name : string
    Condition : Card -> GameState -> bool
    Effect : Card -> Transform<GameState, Command<GameState,PlayerActionType>, PlayerActionType>  }

and AbilityDef = 
    | Action of CardActionDef 
    | Somethingelse

and PlayerActionType = 
    | Pass
    | PlayCharacter of Card
    | Choicei of int * string
    | Choice of string * string
    | YesNoChoice of YesNo * string
    | DeclareAttack of ConflictType * Ring * Card 
    | ChooseAttacker of Card 
    | ChooseDefender of Card
    | ChooseProvince of Card
    | ChooseCharacter of Card * string
    | ChooseCard of Card * string
    | ChooseDynastyToDiscard of Card
    | ActivateAction of CardAction
    | Test    
and CardEffect =
  { Id : int
    Type : EffectType
    Lifetime : Lifetime
    Card : Card }


type GameStateMod = GameState -> GameState


type InitialPlayerConfig = {
    ConflictDeck : CardTitle list
    DynastyDeck : CardTitle list
    Stonghold : CardTitle
    StrongholdProvince : CardTitle
    Provinces : CardTitle list }    
