module Actions

open GameTypes
open GameState

let createChoiceActions nextAction desc min max =
    [min..max]
        |> List.map (fun i -> 
            { Action = nextAction i 
              Type = Choice (i, desc) })

let createYesNoActions nextAction desc =
    [ {Action = nextAction Yes; Type = YesNoChoice (Yes, desc)}
      {Action = nextAction No; Type = YesNoChoice (No, desc)}]

let playerCharctersInPlay ps =
    ps.CardsInPlay 
    |> Map.filter (fun id card -> Card.character card |> Option.isSome)
    |> Map.toList
    |> List.map (fun (id, char) -> char)

let createChooseCharacterInPlayActions nextAction desc gs =
    List.append (playerCharctersInPlay gs.Player1State) (playerCharctersInPlay gs.Player2State) 
    |> List.map (fun c -> 
        {Action = nextAction c; Type = ChooseCharacter (c, desc) })