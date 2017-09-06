module Actions

open GameTypes
open GameState
open PlayerState

let action actionType effect = { Type = actionType; Action = effect}

let choice nextAction desc min max =
    [min..max]
        |> List.map (fun i -> action (Choice (i, desc)) (nextAction i))

let yesNo nextAction desc = 
    [ action (YesNoChoice (Yes, desc)) (nextAction Yes)
      action (YesNoChoice (No, desc)) (nextAction No) ]

let chooseCharacterInPlay nextAction desc gs =
    List.append (playerCharctersInPlay gs.Player1State) (playerCharctersInPlay gs.Player2State) 
    |> List.map (fun c -> 
        {Action = nextAction c; Type = ChooseCharacter (c, desc) })

let pass = action Pass 