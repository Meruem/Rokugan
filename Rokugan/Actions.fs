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
        [for card in ps.CardsInPlayList do
            let cardDef = repository.GetCard card.Title
            for action in CardDef.actions cardDef do
                if action.Condition gs then yield action]
    

    [{ Type =  PlayerActionType.Test
       Player = player
       OnExecute = none ()}]

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
    

let actionWindow gs =
    changes [CleanPassFlags]
    >+> playerActions actions

let onActionPass player gs =
    gs |> changePlayerState player passPlayer

let onCleanPassFlags gs = gs |> cleanPass
