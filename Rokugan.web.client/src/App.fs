module RokuganClient

open Fable.Core
open Fable.Helpers.React
open Elmish
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
open Elmish.React
open Elmish.Debug

open RokuganShared

open Fable.PowerPack
open Fable.PowerPack.Fetch

open Newtonsoft.Json

type Msg = 
    | ChangeState of ServerGameModel
    | QueryState
    | PlayAction of int 
    | FetchFailure of exn
    | NewGame

type CardClientState = 
  { Card : Card
    IsSelected : bool }
    with 
        static member Default card = {Card = card; IsSelected = false}  

type ClientState =
  { Cards : CardClientState list }
    with 
        static member Empty = {Cards = []}

type ClientModel = 
  { Game : ServerGameModel
    Client : ClientState }

let (|PlayerActionOnCard|_|) (action:ServerPlayerAction) =
    match action.Type with
    | PlayCharacter c -> Some (action, c)
    | DeclareAttack (ct,r,c) -> Some (action, c) 
    | ChooseAttacker c -> Some (action, c)
    | ChooseDefender c -> Some (action, c)
    | ChooseProvince c -> Some (action, c)
    | ChooseCharacter (c, des) -> Some (action, c)
    | ChooseCard (c, des) -> Some (action, c)
    | ChooseDynastyToDiscard c -> Some (action, c)
    | _ -> None

let cardClientState clientState (card:Card) =
    match clientState.Cards |> List.tryFind (fun c -> c.Card.Id = card.Id) with
    | Some c -> c
    | None -> CardClientState.Default card

let changeAfter fetch arg = Cmd.ofPromise fetch arg ChangeState FetchFailure
let getGameState () = 
    fetchAs<ServerGameModel> ("/gamestate") [] 
       
let playAction n = 
    fetchAs<ServerGameModel> (sprintf "/play/%d" n) []

let startNewGame () =
   fetchAs<ServerGameModel> ("/newgame") [] 

let init () =
  { Game =
      { State = ServerGameState.Empty
        Log = []
        Actions = [] }
    Client = ClientState.Empty }
    , Cmd.ofMsg QueryState

let update msg model =
    match msg with
    | QueryState -> 
        printfn "query"
        model, changeAfter getGameState ()
    | PlayAction n -> 
        printfn "playAction"
        model, changeAfter playAction n 
    | FetchFailure ex -> 
        printfn "error %A" ex.Message
        model, []
    | ChangeState st ->
        printfn "change state"
        {model with Game = st } , []
    | NewGame -> model, Cmd.ofPromise startNewGame () ChangeState FetchFailure

let actionsView actions dispatch =
    div 
        []
        (actions |> List.map (fun a -> 
            button [ OnClick (fun _ -> dispatch (PlayAction a.Number))] [str <| a.Type.ToString()]))

let cardView (actions : ServerPlayerAction list) (card:CardClientState) dispatch =
    let (Title title) = card.Card.Title
    let cardAction action = match action with | PlayerActionOnCard (a,c) -> card.Card.Id = c.Id  | _ -> false
    let text = sprintf "Card: %s [%d]" title card.Card.Fate
    let el =
        match actions |> List.tryFind cardAction with
        | None -> str <| text
        | Some a -> button [ OnClick (fun _ -> dispatch (PlayAction a.Number))] [str text]  
    
    let borderStr = if card.IsSelected then "5px solid blue" else "5px solid black"
    
    div
        [Style
            [ Width "100px"
              Height "150px"
              Border borderStr]]
        [el]

let playerState model = 
    function
    | Player1 -> model.Game.State.Player1ServerState
    | Player2 -> model.Game.State.Player2ServerState

let getCardView model dispatch card = 
    let cardState = cardClientState model.Client card
    cardView model.Game.Actions cardState dispatch

let playerCards model player = 
    let ps = playerState model player
    ps.CardsInPlay

let cardContainer content =
    div 
        [Style [Display "flex"; FlexDirection "row"; Margin "2"]]
        content

let inPlayView model player dispatch =
    cardContainer
        ([str "InPlay:"]
        @ (playerCards model player |> ofZone ZoneName.Home |> List.map (getCardView model dispatch)))

let dynastyZoneView model player dispatch =
    cardContainer
        ([str "Dynasty:"]
        @ (playerCards model player 
            |> List.filter (fun c -> match c.Zone with | DynastyInProvinces _ -> true | _ -> false)  |> List.map (getCardView model dispatch)))

let provincesView model player dispatch =
    cardContainer
        ([str "Provinces:"]
        @ (playerCards model player |> List.filter (fun c -> match c.Zone with | Province _ -> true | _ -> false)  |> List.map (getCardView model dispatch)))     

let strongholdView model player dispatch = 
    let cards = playerCards model player 
    if cards.Length = 0 then str "" else
        let stronghold = 
            match cards |> ofZone Stronghold with
            | [x] -> x
            | [] -> failwith "no stronghold card"
            | _ -> failwith "too many stronghold cards"
        let strongholdProvince = 
            match cards |> ofZone StrongholdProvince with
            | [x] -> x
            | [] -> failwith "no stronghold province card"
            | _ -> failwith "too many stronghold province cards"
        div
            []
            [ str <| "Stronghold: " 
              getCardView model dispatch stronghold
              getCardView model dispatch strongholdProvince]

let playerView model player invert dispatch =
    let ps = playerState model player
    let maybeInvert lst = if invert then List.rev lst else lst
    let view = 
      [ 
          div
            []
            [ str <| sprintf "Player: %A" player
              str <| sprintf "Honor: %d" ps.Honor
              str <| sprintf "Fate: %d" ps.Fate
              str <| sprintf "Conflict: %d" ps.ConflictDeckCount
              str <| sprintf "Dynasty: %d" ps.DynastyDeckCount ]
          div 
            [ Style [Display "flex"; FlexDirection "row"; Margin "2"]]
            [ strongholdView model player dispatch
              div
                []
                (maybeInvert 
                    [ dynastyZoneView  model player dispatch
                      provincesView model player dispatch ])]
          inPlayView model player dispatch ]
    div 
      []
      (maybeInvert view)

let logView log dispatch = 
    log |> List.map (fun l -> div [] [str l])

let gameStateView model dispatch = 
    div 
      []
      [ str <| sprintf "Turn: %d" model.Game.State.TurnNumber 
        str <| sprintf "Phase: %A" model.Game.State.GamePhase
        str <| sprintf "Active: %A" model.Game.State.ActivePlayer
        playerView model Player1 false dispatch
        playerView model Player2 true dispatch]


let view model dispatch =
    let game = model.Game
    div
        []
        [ div 
              [Style [Position "absolute"; Top "0px"; Left "0px"; Right "80%"; Bottom "0"]]
              ([ button [ OnClick (fun _ -> dispatch NewGame)] [str "New Game"] 
                 actionsView game.Actions dispatch
                 str "Log: "]
              @ (logView game.Log dispatch))
          div 
              [Style [Position "absolute"; Top "0px"; Left "20%"; Right "0"; Bottom "0"]]
              [gameStateView model dispatch]]


//let connection = Debugger.connect (Debugger.Remote("localhost",8000))

Program.mkProgram init update view
|> Program.withReact "elmish-app"
//|> Program.withDebuggerUsing connection
|> Program.run
