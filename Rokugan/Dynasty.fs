module Dynasty

open RokuganShared
open GameTypes
open PlayerState
open GameState
open Actions
open CardRepository

let revealAllDynastyCardsAtProvinces gs =
    List.append gs.Player1State.DynastyInProvinces gs.Player2State.DynastyInProvinces
    |> List.map (Command.removeCardState Hidden) 
    |> List.choose id

let collectFateFromStronghold gs =
    let strongholdDef (ps:PlayerState) = ps.Stronghold.Title |> repository.GetCard
    let fate ps =
        match (strongholdDef ps).Spec with 
        | CardSpec.Stronghold s -> s.FatePerRound
        | _ -> failwith "Stronghold card is not stronghold card"

    let p1Add = fate gs.Player1State
    let p2Add = fate gs.Player2State
    [AddFate (Player1, p1Add); AddFate (Player2, p2Add)]

let add1fateIfPassedFirstMsg gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPassed (playerState otherPl gs) then []
    else 
        [AddFate (gs.ActivePlayer, 1)]

let playDynastyMod card pos addFate player = 
    [ PlayDynasty card
      AddFateOnCard (card, addFate)
      AddFate (player, -addFate)
      DrawDynastyCard (player, pos)
      SwitchActivePlayer ]

let rec dynastyPhaseActions (gs:GameState) =
    let ps = gs.ActivePlayerState
    let actions = 
        getPlayableDynastyPositions ps
        |> List.map (fun (pos, remainingFate) ->
            let card = dynastyCardAtPosition pos ps
            let playCard = 
                fun fate -> 
                        changes (playDynastyMod card pos fate gs.ActivePlayer)
                        >+> playerActions dynastyPhaseActions 
            playCharacter 
                gs.ActivePlayer 
                card 
                (playerActions <| fun gs -> choicei gs.ActivePlayer "Add fate" 0 remainingFate playCard))
    let commands =
        [DynastyPass gs.ActivePlayer] 
        @ (add1fateIfPassedFirstMsg gs) 
        @ [SwitchActivePlayer]
    let passAction = 
        pass 
            gs.ActivePlayer 
            (if hasPlayerPassed (gs.OtherPlayer) gs then 
                changes commands
            else
                changes commands
                >+> playerActions dynastyPhaseActions)
    passAction :: actions     


let gotoDynastyPhase nextPhase (gs:GameState) = 
    changes 
        ([ChangePhase GamePhase.Dynasty]
        @ revealAllDynastyCardsAtProvinces gs  
        @ collectFateFromStronghold gs)
    >+> playerActions     
        dynastyPhaseActions
    >+!> nextPhase

// ------------------------ Message handlers ------------------------

let onDynastyPass player gs =
    gs |> changePlayerState player PlayerState.pass

let onPlayDynastyCard (card:Card) gs =
    let changeState (state:PlayerState) =
        let cardDef = repository.GetCharacterCard card.Title
        state
            |> addFate (-cardDef.Cost)
            |> changeZone Home card
    gs |> GameState.changePlayerState card.Owner changeState    

let onDrawDynastyCard player pos gs = gs |> changePlayerState player (PlayerState.drawCardFromDynastyDeck pos)