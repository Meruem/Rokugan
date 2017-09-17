module Game

open GameTypes

open PlayerState
open GameState
open Dynasty
open Conflict
open Ring
open Draw

// helper function for command handlers which doesn't contain any sub-commands
let send gsmod gs = (gsmod gs), []
let cont = id

let updateState command gm = 
    let (gs2, newcommands : Command list) = 
        gm.State |>
            match command with
            | ChangePhase p -> send <| onChangePhase p
            | RemoveCardState (state, card) -> send <| onRemoveCardState card state 
            | AddFate (player, amount) -> send <| onAddFate player amount 
            | DynastyPass player -> send <| onDynastyPass player 
            | SwitchActivePlayer -> send <| onSwitchActivePlayer
            | PlayDynasty card -> send <| onPlayDynastyCard card
            | AddFateOnCard (card, fate) -> send <| onAddFateToCard fate card
            | DrawDynastyCard (player, pos) -> send <| onDrawDynastyCard player pos
            | Bid (player, amount) -> send <| onPlayerBid player amount
            | ApplyBids -> cont <| onApplyBids
            | CleanBids -> send <| onCleanBids
            | AddHonor (player, amount) -> send <| onAddHonor player amount
            | DrawConflictCard (player, amount) -> send <| onDrawConflictCard player amount
            | PassConflict player -> send <| onPassConflict player
            | DeclareConflict (player, ctype, element, province) -> send <| onConflictDeclared player ctype element province
            | RevealProvince card -> send <| onRevealProvince card
            | DeclareAttacker card -> send <| onAttackerDeclared card
            | DeclareDefender card -> send <| onDefenderDeclared card
            | ConflictStarted -> send <| onConflictStarted
            | CollectFateFromRing (player, ring) -> send <| onCollectFateFromRing player ring 
            | DiscardRandomConflict player -> send <| onDiscardRandomConflict player
            | Bow card -> send <| onCardBow card
            | Ready card -> send <| onCardReady card
            | Honor card -> send <| onCardHonor card
            | Dishonor card -> send <| onCardDishonor card
            | BreakProvince card -> send <| onBreakProvince card
            | DiscardFromPlay card -> send <| onDiscardCardFromPlay card
            | AddFateOnRing (ring, amount) -> send <| onAddFateOnRing ring amount
            | ContestRing ring -> send <| onContestRing ring
            | ClaimRing (player, ring) -> send <| onClaimRing ring player
            | ReturnRing ring -> send <| onReturnRing ring
    ({ gm with Log = command :: gm.Log; State = gs2 }, newcommands)


let rec update transform (gm:GameModel) =
    let commands = transform.Commands
    let nextActions = transform.NextActions
    match commands with 
    | cmd :: xs -> 
        let (gm2, moreCommands) = updateState cmd gm   
        let triggers = gm.Triggers.TryFind (cmd.ToString())
        match triggers with
        | None -> 
            update { Commands = xs @ moreCommands; NextActions = nextActions } gm2
        | Some trgs -> 
            let cnt = {Commands = xs; NextActions = nextActions}  // continuation for current context
            match trgs with
            | [] -> failwith "wtf"
            | first :: rest -> 
                let cnt2 = rest |> List.map (fun trg -> trg.Transform) // other triggers
                let cnt = cnt2 @ [cnt] @ gm.Continuations // push other triggers first, then current context and then remaining continuations
                update first.Transform  {gm2 with Continuations = cnt} 
    | [] -> 
        let actions = nextActions gm.State
        if actions.Length = 0 then // when no action is left, pop the continuation stack and resolve commands
            match gm.Continuations with
            | cnt :: rest -> update cnt {gm with Continuations = rest} 
            | [] -> failwith "expected continuation"
        else { gm with Actions = actions }

let rec gotoNextPhase phase gs =
    match phase with
    | Dynasty -> Dynasty.gotoDynastyPhase (gotoNextPhase Draw) gs
    | Draw -> Draw.gotoDrawPhase (gotoNextPhase Conflict) gs
    | Conflict -> Conflict.gotoConflictPhase (gotoNextPhase Fate) gs
    | Fate -> Fate.gotoFatePhase (gotoNextPhase Regroup) gs
    | Regroup -> Regroup.gotoRegroupPhase (gotoNextPhase Dynasty) gs
    | _ -> { Commands = []; NextActions = fun _ -> []}

let playAction n gm =
    if n >= gm.Actions.Length then gm
    else
        let action = gm.Actions.[n]
        update {Commands = action.Commands; NextActions = action.NextActions} gm

let createStartingRings = 
  [ Ring.createRing Element.Fire
    Ring.createRing Element.Water
    Ring.createRing Element.Air
    Ring.createRing Element.Earth
    Ring.createRing Element.Void ]

let startGame playerConfig1 playerConfig2 firstPlayer = 
    let gs =
        { 
            AttackState = None
            Rings = createStartingRings
            TurnNumber = 1
            ActivePlayer = firstPlayer
            GamePhase = Dynasty
            FirstPlayer = firstPlayer
            Player1State = initializePlayerState playerConfig1 Player1
            Player2State = initializePlayerState playerConfig2 Player2 }
        |> addSecondPlayer1Fate
      //  |> Triggers.addWinConditionsTriggers
      //  |> Triggers.addDynastyPassTrigger gotoNextPhase
     //   |> Triggers.addConflictEndTrigger gotoNextPhase
    let gm =
        { State = gs 
          Actions = [] 
          Continuations = []
          Log = []
          Triggers = Map.empty}
    update (gotoNextPhase Dynasty gs) gm 
