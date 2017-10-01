module ClientConvert

open RokuganShared
open GameTypes

let toClientPlayerState (ps:PlayerState) =
  { ClientPlayerState.Bid = ps.Bid
    ConflictDeckCount = ps.ConflictDeck.Cards.Length
    DynastyDeckCount = ps.DynastyDeck.Cards.Length
    Honor = ps.Honor
    Fate = ps.Fate
    CardsInPlay = ps.CardsInPlayList
    DeclaredConflicts = ps.DeclaredConflicts }

let toClientGameState (gs : GameState) = 
  { TurnNumber = gs.TurnNumber
    FirstPlayer = gs.FirstPlayer
    Player1ClientState = toClientPlayerState gs.Player1State
    Player2ClientState = toClientPlayerState gs.Player2State
    Rings = gs.Rings
    GamePhase = gs.GamePhase
    ActivePlayer = gs.ActivePlayer
    AttackState = gs.AttackState }

let toClientPlayerActions (actions : PlayerAction list) = 
    actions
    |> List.mapi (fun i a -> 
        { Number = i
          Type = a.Type
          Player = a.Player.ToString()})    