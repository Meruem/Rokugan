module Dynasty

open RokuganShared
open GameTypes
open PlayerState
open GameState
open PlayerActions
open CardRepository

let revealAllDynastyCardsAtProvinces gs =
    gs.Player1State.DynastyInProvinces @ gs.Player2State.DynastyInProvinces
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

let switchActivePlayer gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPlayerPassed otherPl gs then []
    else [SwitchActivePlayer]
    

let playDynastyMod card pos addFate player = 
    [ PlayDynasty card
      AddFateOnCard (card, addFate)
      AddFate (player, -addFate)
      DrawDynastyCard (player, pos) ]


let rec dynastyPhaseActions (gs:GameState) =
    let ps = gs.ActivePlayerState
    let actions = 
        getPlayableDynastyPositions ps
        |> List.map (fun (pos, remainingFate) ->
            let card = dynastyCardAtPosition pos ps
            let playCard = 
                fun fate -> 
                        changes (playDynastyMod card pos fate gs.ActivePlayer)
                        >+> act switchActivePlayer
                        >+> playerActions dynastyPhaseActions "Dynasty phase:"
            playCharacter 
                gs.ActivePlayer 
                card 
                (playerActions (fun gs -> choicei gs.ActivePlayer "Add fate" 0 remainingFate playCard) "Dynasty phase:"))
    let passAction = 
        let transform =
            changes [DynastyPass gs.ActivePlayer] 
            >+> act add1fateIfPassedFirstMsg 
            >+> act switchActivePlayer
        pass 
            gs.ActivePlayer 
            (if hasPlayerPassed (gs.OtherPlayer) gs then 
                transform
            else
                transform
                >+> playerActions dynastyPhaseActions "Dynasty phase:")
    passAction :: actions     


let gotoDynastyPhase nextPhase = 
    changes [ChangePhase GamePhase.Dynasty]
    >+> act revealAllDynastyCardsAtProvinces  
    >+> act collectFateFromStronghold 
    >+> playerActions dynastyPhaseActions "Dynasty phase:"
    >+!> nextPhase

// ------------------------ Message handlers ------------------------

let onDynastyPass player gs =
    gs |> changePlayerState player PlayerState.passPlayer

let onPlayDynastyCard (card:Card) gs =
    let changeState (state:PlayerState) =
        let cardDef = repository.GetCharacterCard card.Title
        state
            |> addFate (-cardDef.Cost)
            |> changeCardZone Home card
    gs |> GameState.changePlayerState card.Owner changeState    

let onDrawDynastyCard player pos gs = gs |> changePlayerState player (PlayerState.drawCardFromDynastyDeck pos)
