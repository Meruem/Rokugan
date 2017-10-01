namespace RokuganShared

type Player = Player1 | Player2
type YesNo = Yes | No
type Element = Fire | Water | Air | Earth | Void
type GameEnd = Player1Won | Player2Won 
type GamePhase = Dynasty | Draw | Conflict | Fate | Regroup | End of GameEnd 
type ConflictType = Military | Political
type RingState = Unclaimed | Contested | Claimed of Player
type CardTitle = Title of string
type CardState = Bowed | Honored | Dishonored | Hidden | Broken
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

[<StructuredFormatDisplay("{Element}/{State}/({Fate})")>]
type Ring = 
  { Element : Element
    State : RingState
    Fate : int }

[<StructuredFormatDisplayAttribute("Card {Id} [{Title}] in {Zone} (+{Fate})")>]
type Card = {
    Id : int
    Title : CardTitle
    Owner : Player
    States : CardState Set
    Fate : int 
    Zone : ZoneName }    

type AttackState =
  { Type : ConflictType
    Attacker : Player
    Ring : Ring
    Province : Card
    Attackers : Card list
    Defenders : Card list }
    with
        member this.Defender = match this.Attacker with | Player1 -> Player2 | Player2 -> Player1   

type ClientPlayerState = {
    Bid : int option
    ConflictDeckCount : int
    DynastyDeckCount : int
    Honor : int
    Fate : int
    CardsInPlay : Card list
    DeclaredConflicts : ConflictType option list }
    with
        static member Empty =
          { Bid = None
            ConflictDeckCount = 0
            DynastyDeckCount = 0
            Honor = 0
            Fate = 0
            CardsInPlay = []
            DeclaredConflicts = [] }

type PlayerActionType = 
    | Pass
    | PlayCharacter of Card
    | ActivateAction
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
    | Test

type ClientGameState = 
  { TurnNumber : int
    FirstPlayer : Player
    Player1ClientState : ClientPlayerState
    Player2ClientState : ClientPlayerState 
    Rings : Ring list
    GamePhase : GamePhase
    ActivePlayer : Player
    AttackState : AttackState option }
    with
        static member Empty = 
          { TurnNumber = 0
            FirstPlayer = Player1
            Player1ClientState = ClientPlayerState.Empty
            Player2ClientState = ClientPlayerState.Empty 
            Rings = []
            GamePhase = GamePhase.Dynasty
            ActivePlayer = Player1
            AttackState = None }

type ClientPlayerAction = { 
    Number : int
    Type : PlayerActionType
    Player : string }

type ClientModel = {
    State : ClientGameState
    Log : string list
    Actions : ClientPlayerAction list }    

[<AutoOpen>]
module Cards =

    let ofZone zone  = 
        List.filter (fun c -> c.Zone = zone)