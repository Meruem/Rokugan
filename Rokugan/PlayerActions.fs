/// Module for player actions
module PlayerActions

open GameTypes
open RokuganShared

open GameState
open PlayerState

let private chooseCharacterAction player desc next card = action player (ChooseCharacter (card, desc)) (next card)
let private chooseCardAction player desc next card = action player (ChooseCard (card, desc)) (next card)

let chooseDynastyToDiscard player next card = action player (ChooseDynastyToDiscard card) (next card)

let chooseCharacterInPlay player desc next gs =
    List.append (charactersInPlay gs.Player1State) (charactersInPlay gs.Player2State) 
    |> List.map (chooseCharacterAction player desc next)

let chooseCard player condition desc next (gs:GameState) =
    gs.Cards 
    |> List.filter condition
    |> List.map (chooseCardAction player desc next)

let chooseCharacter player condition desc getTransform (gs:GameState) =
    gs.Cards 
    |> List.filter (fun c -> condition c && Card.isCharacter c)
    |> List.map (chooseCharacterAction player desc getTransform)

let pass player = action player Pass

//gets list of actions for both players for each dynasty card to be chosen
//each action has continuation pointing again at this function
//after both players bassed "next" function is called with parameter list of all chosen cards 
let rec chooseDynastyInProvince next =
    let rec chooseDynastyInProvinceRec next chosen passed  =
        let pl1Passed = List.contains Player1 passed
        let pl2Passed = List.contains Player2 passed
        let passPl1 () = 
            if pl2Passed then pass Player1 (next chosen)
            else pass Player1 (chooseDynastyInProvinceRec next chosen (Player1::passed))
        let passPl2 () = 
            if pl1Passed then pass Player2 (next chosen)
            else pass Player2 (chooseDynastyInProvinceRec next chosen (Player2::passed) )
        let chooseCard card = chooseDynastyInProvinceRec next (card::chosen) passed
        let actions (state:PlayerState) =
            state.DynastyInProvinces
            |> List.filter (fun c -> not (Card.isHidden c) && not (chosen |> List.contains c))
            |> List.map  (fun c -> chooseDynastyToDiscard c.Owner chooseCard c)
        playerActions
            (fun gs ->
                if pl1Passed then [] else [passPl1 ()]
                    @ if pl2Passed then [] else [passPl2 ()]
                    @ if pl1Passed then [] else actions gs.Player1State
                    @ if pl2Passed then [] else actions gs.Player2State)
            "Choose dynasty card:"        
    chooseDynastyInProvinceRec next [] [] 

let playCharacter player card = action player (PlayCharacter card)

let chooseProvince player province = action player (ChooseProvince province) 

let declareAttack attacker ct ring prov = action attacker (DeclareAttack (ct, ring, prov))