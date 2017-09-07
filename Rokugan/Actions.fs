module Actions

open GameTypes
open GameState
open PlayerState

let action actionType effect = { Type = actionType; Action = effect}

// creates multiple choice action from min to max
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

let chooseDynastyToDiscard next card = action (ChooseDynastyToDiscard card) (next card)

let chooseCharacterInPlay desc next gs =
    List.append (charactersInPlay gs.Player1State) (charactersInPlay gs.Player2State) 
    |> List.map (chooseCharacterAction desc next)

let chooseCard condition desc next (gs:GameState) =
    gs.Cards 
    |> List.filter condition
    |> List.map (chooseCardAction desc next)

let chooseCharacter condition desc next (gs:GameState) =
    gs.Cards 
    |> List.filter (fun c -> condition c && Card.isCharacter c)
    |> List.map (chooseCharacterAction desc next)

let pass player = action (Pass player) 

let rec chooseDynastyInProvince next (gs:GameState) =
    let rec chooseDynastyInProvinceRec next chosen passed (gs:GameState) =
        let pl1Passed = List.contains Player1 passed
        let pl2Passed = List.contains Player2 passed
        let passPl1 = 
            if pl2Passed then pass Player1 (next chosen)
            else pass Player1 (setActions (chooseDynastyInProvinceRec next chosen (Player1::passed) gs))
        let passPl2 = 
            if pl1Passed then pass Player2 (next chosen)
            else pass Player2 (setActions (chooseDynastyInProvinceRec next chosen (Player2::passed) gs))
        let chooseCard card gs = gs >!=> chooseDynastyInProvinceRec next (card::chosen) passed gs
        let actions (state:PlayerState) =
            state.DynastyInProvinces
            |> List.filter (fun c -> not (Card.isHidden c) && not (chosen |> List.contains c))
            |> List.map  (chooseDynastyToDiscard chooseCard)
        if pl1Passed then [] else [passPl1]
            @ if pl2Passed then [] else [passPl2]
            @ if pl1Passed then [] else actions gs.Player1State
            @ if pl2Passed then [] else actions gs.Player2State
    chooseDynastyInProvinceRec next [] [] gs

let playCharacter title = action (PlayCharacter title)

let chooseProvince province = action (ChooseProvince province) 


let declareAttack ctype element = action (DeclareAttack (ctype,element))