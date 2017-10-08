module Actions

open RokuganShared
open GameTypes
open PlayerState
open GameState
open PlayerActions

let getAllPlayableActions player gs  =
    [{ Type =  PlayerActionType.Test
       Player = player
       OnExecute = none}]

// let actionWindow gs =
//     if hasPassed gs.Player1State && hasPassed gs.Player2State then none
//     else 
//         let passAction = 
//             pass 
//                 gs.ActivePlayer 
//                 (changes 
//                     [ ActionPass gs.ActivePlayer
//                       SwitchActivePlayer]
//                  >+> actionWindow      ) 

let onActionPass player gs =
    gs |> changePlayerState player passPlayer