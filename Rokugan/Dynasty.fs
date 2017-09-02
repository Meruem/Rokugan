module Dynasty

open GameTypes
open PlayerState
open GameState

let revealAllDynastyCardsAtProvinces gs =
    let removeHiddenState = Card.removeCardState Hidden
    let removeHiddenFromPlayerState pState =
        { pState with DynastyInProvinces = pState.DynastyInProvinces.Cards |> List.map removeHiddenState |> Zone }
    { gs with 
        Player1State = removeHiddenFromPlayerState gs.Player1State
        Player2State = removeHiddenFromPlayerState gs.Player2State }

let playDynastyCard position addFate gs =
    let changeState state =
        let card = state.DynastyInProvinces.Cards.[position]
        let card' = { card with Fate = addFate}
        let home = card' :: state.Home.Cards
        let newCard, state' = PlayerState.drawCardFromDynastyDeck state
        let cardDef = CardRepository.getCharacterCard card.Title
        // add new dynasty card
        let dynastyInProvinces' = 
            state.DynastyInProvinces.Cards 
            |> Utils.replaceListElement newCard position
        {state' with 
            Home = Zone home
            DynastyInProvinces = Zone dynastyInProvinces'
            Fate = state'.Fate - cardDef.Cost - addFate}
    gs |> GameState.changeActivePlayerState changeState    

let collectFateFromStronghold gs =
    let stronghold (pState:PlayerState) = pState.Stonghold.StrongholdCard.Title |> CardRepository.getCard
    let fate pState =
        match (stronghold pState).Spec with 
        | Stronghold s -> s.FatePerRound
        | _ -> failwith "Stronghold card is not stronghold card"

    let p1Add = fate gs.Player1State
    let p2Add = fate gs.Player2State
    { gs with 
        Player1State = (PlayerState.addFateToPlayer p1Add gs.Player1State)    
        Player2State = (PlayerState.addFateToPlayer p2Add gs.Player2State)  }        

let add1fateIfPassedFirst gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPassed (getPlayerState otherPl gs) then gs
    else 
        let add1Fate (state:PlayerState) = {state with Fate = state.Fate + 1} 
        gs |> changeActivePlayerState add1Fate

let rec addDynastyPhaseActions gs =
    let playerState = GameState.activePlayerState gs
    let actions = 
        getPlayableDynastyPositions playerState
        |> List.map (fun (pos, remainingFate) ->
            let nextAction fate = playDynastyCard pos fate >> GameState.switchActivePlayer >> addDynastyPhaseActions
            { 
                Action = createChoiceActions nextAction 0 remainingFate |> addChoiceActions
                Type = PlayCharacter playerState.DynastyInProvinces.Cards.[pos].Title })
    if PlayerState.hasPassed playerState then { gs with Actions = actions}
    else 
        let passAction = 
            { Type = PlayerActionType.Pass 
              Action = passActive >> add1fateIfPassedFirst >> switchActivePlayer >> addDynastyPhaseActions } 
        let actions' = passAction :: actions
        { gs with Actions = actions'}     

        