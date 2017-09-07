module Dynasty

open GameTypes
open PlayerState
open GameState
open Actions

let revealAllDynastyCardsAtProvinces gs =
    let removeHiddenState = Card.removeCardState Hidden
    let ps1 = gs.Player1State |> changePlayerCards gs.Player1State.DynastyInProvinces removeHiddenState
    let ps2 = gs.Player2State |> changePlayerCards gs.Player2State.DynastyInProvinces removeHiddenState
    {gs with Player1State = ps1; Player2State = ps2}

let playDynastyCard position additionalFate gs =
    let changeState (state:PlayerState) =
        let dynastyCard = 
            state
            |> dynastyCardAtPosition position
            |> Card.putAddtitionalFate additionalFate
        let newCard, state' = PlayerState.drawCardFromDynastyDeck state
        let cardDef = CardRepository.getCharacterCard dynastyCard.Title
        state' 
            |> addFate (-cardDef.Cost - additionalFate)
            |> addCardToPlay dynastyCard Home
            |> addCardToPlay newCard (DynastyInProvinces (Card.dynastyCardPosition dynastyCard))
    gs |> GameState.changeActivePlayerState changeState    

let collectFateFromStronghold gs =
    let strongholdDef (ps:PlayerState) = ps.Stronghold.Title |> CardRepository.getCard
    let fate ps =
        match (strongholdDef ps).Spec with 
        | CardSpec.Stronghold s -> s.FatePerRound
        | _ -> failwith "Stronghold card is not stronghold card"

    let p1Add = fate gs.Player1State
    let p2Add = fate gs.Player2State
    { gs with 
        Player1State = (PlayerState.addFateToPlayer p1Add gs.Player1State)    
        Player2State = (PlayerState.addFateToPlayer p2Add gs.Player2State)  }        

let add1fateIfPassedFirst gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPassed (playerState otherPl gs) then gs
    else 
        let add1Fate (state:PlayerState) = {state with Fate = state.Fate + 1} 
        gs |> changeActivePlayerState add1Fate

let rec addDynastyPhaseActions (gs:GameState) =
    let ps = gs.ActivePlayerState
    let chooseAddFate nextAction remainingFate gs = 
        gs >!=> choicei "Add fate" 0 remainingFate nextAction
    let actions = 
        getPlayableDynastyPositions ps
        |> List.map (fun (pos, remainingFate) ->
            let playCard fate = 
                playDynastyCard pos fate 
                >> GameState.switchActivePlayer 
                >> addDynastyPhaseActions
            playCharacter ((dynastyCardAtPosition pos ps).Title) (chooseAddFate playCard remainingFate) )
    if PlayerState.hasPassed ps then gs >!=> actions
    else 
        let passAction = 
            pass (passActive 
                >> add1fateIfPassedFirst 
                >> switchActivePlayer 
                >> addDynastyPhaseActions)
        gs >!=> [passAction] >+=> actions     