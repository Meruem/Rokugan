module Actions

open RokuganShared
open GameTypes
open PlayerState
open GameState
open PlayerActions
open CardRepository

let getAllPlayableActions player gs  =
    let ps = gs |> playerState player 
    let actions =
        [for action in gs.CardActions do
            if action.Spec.Condition action.Card gs then yield action]
    
    actions 
    |> List.map (fun action ->
        { Type =  PlayerActionType.ActivateAction action
          Player = player
          OnExecute = action.Spec.Effect action.Card})

let rec actions gs =
    if hasPassed gs.Player1State && hasPassed gs.Player2State then []
    else
        let passAction = 
            pass 
                gs.ActivePlayer 
                (changes 
                    [ ActionPass gs.ActivePlayer
                      SwitchActivePlayer]
                 >+> playerActions actions) 
        passAction :: (getAllPlayableActions gs.ActivePlayer gs)
    

let actionWindow startingPlayer =
    changes [CleanPassFlags]
    >+> act (fun gs -> 
        match startingPlayer with 
        | ActionWindowStarts.FirstPlayer -> [SetActivePlayer gs.FirstPlayer] 
        | Defender ->
            match gs.AttackState with 
            | Some state -> [SetActivePlayer state.Defender] 
            | None -> failwith "called action window with 'Defender' but outside combat" )
    >+> playerActions actions

let onActionPass player gs =
    gs |> changePlayerState player passPlayer

let onCleanPassFlags gs = gs |> cleanPass
