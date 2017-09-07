module Fate

open GameTypes
open GameState

let remove1FateorDiscard (card:Card) = 
    if card.Fate > 0 then Card.putAdditionalFate -1 card
    else Card.discard card

let fatePhase gotoNextPhase (gs:GameState) =
    let chars = 
        gs.Cards 
        |> List.filter (fun c -> Card.isCharacter c && c.Zone = Home)
    gs |> changeCards remove1FateorDiscard chars |> gotoNextPhase

let gotoFatePhase gotoNextPhase gs =
    { gs with
        GamePhase = Fate
        ActivePlayer = gs.FirstPlayer }
    |> fatePhase gotoNextPhase