module Actions

open RokuganShared
open GameTypes
open PlayerState
open GameState
open PlayerActions
open CardRepository


let rec actions prompt gs =
    if hasPassed gs.Player1State && hasPassed gs.Player2State then []
    else
        let getAllPlayableActions gs  =
            let ps = gs |> activePlayerState 
            let player = gs.ActivePlayer
            let cardActions =
                [for action in gs.CardActions do
                    if action.Card.Owner = player && action.Spec.Condition action.Card gs then yield action]

            cardActions 
            |> List.map (fun action ->
                { Type =  PlayerActionType.ActivateAction action
                  Player = player
                  OnExecute = 
                      action.Spec.Effect action.Card
                      >+> changes [CleanPassFlags; SwitchActivePlayer]
                      >+> playerActions (actions prompt) prompt})
        let passAction = 
            pass 
                gs.ActivePlayer 
                (changes 
                    [ ActionPass gs.ActivePlayer
                      SwitchActivePlayer]
                 >+> playerActions (actions prompt) prompt) 
        let cardActions = 
            getAllPlayableActions gs
        passAction :: cardActions
    

let actionWindow startingPlayer prompt =
    changes [CleanPassFlags]
    >+> act (fun gs -> 
        match startingPlayer with 
        | ActionWindowStarts.FirstPlayer -> [SetActivePlayer gs.FirstPlayer] 
        | Defender ->
            match gs.AttackState with 
            | Some state -> [SetActivePlayer state.Defender] 
            | None -> failwith "called action window with 'Defender' but outside combat" )
    >+> playerActions (actions prompt) prompt

let cardAction name condition effect card =
  { Spec =  
      { Name = name
        Condition = condition
        Effect = effect }
    Card = card
    LastUsed = None }



let addCardActions (card:Card) (gm:GameModel<GameState, Command, PlayerActionType>) = 
    let cardDef = CardRepository.repository.GetCard card.Title
    let newActions =
        cardDef.Actions
        |> List.map (fun ac ->
            cardAction 
                (sprintf "%A[%d]: %s" card.Title card.Id ac.Name)
                ac.Condition
                ac.Effect
                card)
    let state' = { gm.State with CardActions = newActions @ gm.State.CardActions }
    {gm with State = state' }

let addAllCardsActions (gm:GameModel<GameState, Command, PlayerActionType>) = 
    gm.State.Cards 
    |> List.fold (fun acc card -> addCardActions card acc) gm

let onActionPass player gs =
    gs |> changePlayerState player passPlayer

let onCleanPassFlags gs = gs |> cleanPass
