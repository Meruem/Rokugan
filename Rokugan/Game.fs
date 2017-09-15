module Game

open GameTypes

open PlayerState
open GameState
open Dynasty
open Conflict

let updateState command gm = 
    let gs2 = 
        gm.State 
        |> match command with
            | ChangePhase p -> onChangePhase p
            | RemoveCardState (state, card) -> onRemoveCardState card state 
            | AddFate (player, amount) -> onAddFate player amount 
            | DynastyPass player -> onDynastyPass player 
            | SwitchActivePlayer -> onSwitchActivePlayer
            | PlayDynasty card -> onPlayDynastyCard card
            | AddFateOnCard (card, fate) -> onAddFateToCard fate card
            | DrawDynastyCard (player, pos) -> onDrawDynastyCard player pos
            | Bid (player, amount) -> onPlayerBid player amount
            | CleanBids -> onCleanBids
            | AddHonor (player, amount) -> onAddHonor player amount
            | DrawConflictCard (player, amount) -> onDrawConflictCard player amount
            | PassConflict player -> onPassConflict player
            | DeclareConflict (player, ctype, element, province) -> onConflictDeclared player ctype element province
            | RevealProvince card -> onRevealProvince card
            | DeclareAttacker card -> onAttackerDeclared card
            | DeclareDefender card -> onDefenderDeclared card
            | ConflictStarted -> onConflictStarted
            | CollectFateFromRing (player, ring) -> onCollectFateFromRing player ring 
            | DiscardRandomConflict player -> onDiscardRandomConflict player
            | Bow card -> onCardBow card
            | Ready card -> onCardReady card
            | Honor card -> onCardHonor card
            | Dishonor card -> onCardDishonor card
            | BreakProvince card -> onBreakProvince card
    { gm with Log = command :: gm.Log; State = gs2 }


let rec updateM commands (gm:GameModel) (nextActions:GameState -> PlayerAction list) =
    match commands with 
    | cmd :: xs -> 
        let gm2 = updateState cmd gm   
        let triggers = gm.Triggers.TryFind (cmd.ToString())
        match triggers with
        | None -> updateM xs gm2 nextActions
        | Some trgs -> 
            let cnt = {Commands = xs; NextActions = nextActions}  // continuation for current context
            match trgs with
            | [] -> failwith "wtf"
            | first :: rest -> 
                let cnt2 = rest |> List.map (fun trg -> {Commands = trg.Commands; NextActions = trg.NextActions}) // other triggers
                let cnt = cnt2 @ [cnt] @ gm.Continuations // push other triggers first, then current context and then remaining continuations
                updateM first.Commands {gm2 with Continuations = cnt} first.NextActions
    | [] -> 
        let actions = nextActions gm.State
        if actions.Length = 0 then // when no action is left, pop the continuation stack and resolve commands
            match gm.Continuations with
            | cnt :: rest -> updateM cnt.Commands {gm with Continuations = rest} cnt.NextActions
            | [] -> failwith "expected continuation"
        else { gm with Actions = actions }

let rec update getCommands gs = 
    updateM getCommands gs

let rec gotoNextPhase phase gs =
    match phase with
    | Draw -> {Commands = Draw.gotoDrawPhase gs; NextActions = Draw.drawPhaseActions (gotoNextPhase Conflict gs) }
    | Conflict -> {Commands = Conflict.gotoConflictPhase; NextActions = Conflict.conflictActions (gotoNextPhase Fate gs)}
    | _ -> {Commands = []; NextActions = fun _ -> [] } //temorary
  //  | Conflict -> gs' |> Fate.gotoFatePhase gotoNextPhase
  //  | Fate -> gs' |> Regroup.gotoRegroupPhase gotoNextPhase
  //  | Regroup -> gs' |> nextRound |> Dynasty.gotoDynastyPhase
  //  | _ -> gs'

let playAction n gm =
    if n >= gm.Actions.Length then gm
    else
        let action = gm.Actions.[n]
        updateM action.Commands gm action.NextActions

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
    //let gs' = update Dynasty.gotoDynastyPhase (Dynasty.dynastyPhaseActions gs)
    let gm =
        { State = gs 
          Actions = [] 
          Continuations = []
          Log = []
          Triggers = Map.empty}
    updateM (Dynasty.gotoDynastyPhase gs) gm (Dynasty.dynastyPhaseActions (gotoNextPhase Draw))
