module Actions

open GameTypes
open GameState
open PlayerState

let action actionType effect = { Type = actionType; Action = effect}

let choicei desc min max next =
    [min..max]
        |> List.map (fun i -> action (Choicei (i, desc)) (next i))

let choice desc xs next = 
    xs |> List.map (fun s -> action (Choice (s, desc)) (next s))

let yesNo desc next = 
    [ action (YesNoChoice (Yes, desc)) (next Yes)
      action (YesNoChoice (No, desc)) (next No) ]

let private chooseCharacterAction desc next card = action (ChooseCharacter (card, desc)) (next card)
let private chooseCardAction desc next card = action (ChooseCard (card, desc)) (next card)

let chooseCharacterInPlay desc next gs =
    List.append (charactersInPlay gs.Player1State) (charactersInPlay gs.Player2State) 
    |> List.map (chooseCharacterAction desc next)

let chooseCard condition desc next (gs:GameState) =
    gs.Cards 
    |> List.filter condition
    |> List.map (chooseCardAction desc next)

let chooseCharacter condition desc next (gs:GameState) =
    gs.Cards 
    |> List.filter (fun c -> condition c &&  Card.character c |> Option.isSome)
    |> List.map (chooseCharacterAction desc next)

let pass = action Pass 

let playCharacter title = action (PlayCharacter title)

let chooseProvince province = action (ChooseProvince province) 


let declareAttack ctype element = action (DeclareAttack (ctype,element))