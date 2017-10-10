module ClientConvert

open RokuganShared
open GameTypes

let toClientPlayerState (ps:PlayerState) =
  { ServerPlayerState.Bid = ps.Bid
    ConflictDeckCount = ps.ConflictDeck.Cards.Length
    DynastyDeckCount = ps.DynastyDeck.Cards.Length
    Honor = ps.Honor
    Fate = ps.Fate
    CardsInPlay = ps.CardsInPlayList
    DeclaredConflicts = ps.DeclaredConflicts }

let toClientGameState (gs : GameState) = 
  { TurnNumber = gs.TurnNumber
    FirstPlayer = gs.FirstPlayer
    Player1ServerState = toClientPlayerState gs.Player1State
    Player2ServerState = toClientPlayerState gs.Player2State
    Rings = gs.Rings
    GamePhase = gs.GamePhase
    ActivePlayer = gs.ActivePlayer
    AttackState = gs.AttackState }

let toClientPlayerActions (actions : PlayerAction<'a, 'b, 'c> list) = 
    actions
    |> List.mapi (fun i a -> 
        { Number = i
          Type = a.Type.ToString()
          Player = a.Player.ToString()})    