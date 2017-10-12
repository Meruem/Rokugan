module Game

open GameTypes
open RokuganShared

open PlayerState
open GameState
open Dynasty
open Conflict
open Ring
open Draw
open Actions

// helper function for command handlers which doesn't contain any sub-commands
let send gsmod gs = (gsmod gs), []
let cont = id

let onEndGame status (gs:GameState) =
    {gs with GamePhase = GamePhase.End status}

let updateState command (gm:GameModel<GameState, Command, PlayerActionType>) = 
    let (gs2, newcommands) = 
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
            | NextRound -> send <| onNextRound
            | EndGame status -> send <| onEndGame status
            | ActionPass player -> send <| onActionPass player
            | SetActivePlayer player -> send <| onSetActivePlayer player
            | CleanPassFlags -> send <| onCleanPassFlags
            | SetFirstPlayerActive -> send <| onSetActivePlayer gm.State.FirstPlayer
            | Debug str -> send <| (fun gs -> (printfn "Debug: %s" str); gs)
    ({ gm with Log = command :: gm.Log; State = gs2 }, newcommands)

let rec update t (updateState:'cmd-> GameModel<'gs,'cmd, 'pa> -> (GameModel<'gs,'cmd, 'pa> * 'cmd list)) (gm:GameModel<'gs, 'cmd, 'pa>) =
    let cmds = 
        match t.Commands with
        | Some getCommands -> getCommands gm.State
        | None -> [] 
    match cmds with 
    | cmd :: xs -> 
        // pick first command and update the game state
        let (gm2, moreCommands) = updateState cmd gm   
        // update can return additional commands so add them to rest of commands
        let nextTransform = {t with Commands = Some (fun _ -> xs) }
        let trgs = 
            gm2.Triggers 
            |> List.filter (fun t -> t.Condition cmd gm2.State) //gm.State.Triggers.TryFind (cmd.ToString())
        match trgs with
        | [] -> update nextTransform updateState gm2
        | trigger :: rest -> 
            let cnt2 = rest |> List.map (fun trg -> (fun () -> trg.Transform)) // other triggers
            let cnt3 = cnt2 @ [fun () -> nextTransform] @ gm.Continuations // push other triggers first, then current context and then remaining continuations
            let gm3 = 
                if trigger.Lifetime = Once then 
                    gm2 |> Triggers.removeTrigger trigger.Name
                else gm2 
            update trigger.Transform  updateState {gm3 with Continuations = cnt3} 
    | [] -> 
        // no more commands
        // if there is continuation -> push tu stack
        let gm' = 
            match t.Continuation with
            | _::_ -> {gm with Continuations = t.Continuation @ gm.Continuations}
            | [] -> gm
        // if not action is defined pop continuation 
        let updateContinuation gm =
            match gm.Continuations with
            | [] -> gm
            | cnt :: rest -> update (cnt()) updateState {gm with Continuations = rest} 

        match t.NextActions with
        | Some getactions -> 
            let newActions = getactions gm'.State
            match newActions with
            | _ :: _ -> {gm' with Actions = newActions}
            | [] -> gm' |> updateContinuation  // empty player action -> pop continuation
        | None -> 
            gm' |> updateContinuation

let rec gotoNextPhase phase =
    let nextPhaseLazy phase = fun () -> gotoNextPhase phase
    match phase with
    | GamePhase.Dynasty -> Dynasty.gotoDynastyPhase (nextPhaseLazy Draw)
    | GamePhase.Draw -> Draw.gotoDrawPhase (nextPhaseLazy GamePhase.Conflict)
    | GamePhase.Conflict -> Conflict.gotoConflictPhase (nextPhaseLazy Fate)
    | GamePhase.Fate -> Fate.gotoFatePhase (nextPhaseLazy Regroup)
    | GamePhase.Regroup -> Regroup.gotoRegroupPhase (nextPhaseLazy Dynasty)
    | _ -> transform None None []

let playAction n (gm:GameModel<GameState, Command, PlayerActionType>) =
    if n >= gm.Actions.Length then gm
    else
        let action = gm.Actions.[n]
        update action.OnExecute updateState gm

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
            CardActions = []
            Player1State = initializePlayerState playerConfig1 Player1
            Player2State = initializePlayerState playerConfig2 Player2 }
        |> addSecondPlayer1Fate
    let gm =
        { State = gs 
          Actions = [] 
          Continuations = []
          Triggers = []
          Log = []}
        |> Triggers.addWinConditionsTriggers
        |> Triggers.addAllCardsTriggers
    update (gotoNextPhase Dynasty) updateState gm 
