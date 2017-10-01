module RokuganClient

open Fable.Core
open Fable.Helpers.React
open Elmish
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
open Elmish.React
open Elmish.Debug

open GameTypes
open RokuganShared

open Fable.PowerPack
open Fable.PowerPack.Fetch

open Newtonsoft.Json

type Msg = 
    | ChangeState of ClientModel
    | QueryState
    | PlayAction of int 
    | FetchFailure of exn
    | NewGame

let (|PlayerActionOnCard|_|) (action:ClientPlayerAction) =
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

let changeAfter fetch arg = Cmd.ofPromise fetch arg ChangeState FetchFailure
let getGameState () = 
    fetchAs<ClientModel> ("/gamestate") [] 
       
let playAction n = 
    fetchAs<ClientModel> (sprintf "/play/%d" n) []

let startNewGame () =
   fetchAs<ClientModel> ("/newgame") [] 

let init () =
  { State = ClientGameState.Empty
    Log = []
    Actions = [] }, Cmd.ofMsg QueryState

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
        st, []
    | NewGame -> model, Cmd.ofPromise startNewGame () ChangeState FetchFailure

let actionsView actions dispatch =
    div 
        []
        (actions |> List.map (fun a -> 
            button [ OnClick (fun _ -> dispatch (PlayAction a.Number))] [str <| a.Type.ToString()]))

let cardView (actions : ClientPlayerAction list) dispatch (card:Card)=
    let (Title title) = card.Title
    let cardAction action = match action with | PlayerActionOnCard (a,c) -> card.Id = c.Id  | _ -> false
    let text = sprintf "Card: %s [%d]" title card.Fate
    let el =
        match actions |> List.tryFind cardAction with
        | None -> str <| text
        | Some a -> button [ OnClick (fun _ -> dispatch (PlayAction a.Number))] [str text]  

    div
        []
        [el]

let inPlayView cards (actions : ClientPlayerAction list) dispatch =
    div
        []
        ([str "InPlay:"]
        @ (cards |> ofZone ZoneName.Home |> List.map (cardView actions dispatch)))

let dynastyZoneView cards (actions : ClientPlayerAction list) dispatch =
    div
        []
        ([str "Dynasty:"]
        @ (cards |> List.filter (fun c -> match c.Zone with | DynastyInProvinces _ -> true | _ -> false)  |> List.map (cardView actions dispatch)))

let provincesView cards (actions : ClientPlayerAction list) dispatch =
    div
        []
        ([str "Provinces:"]
        @ (cards |> List.filter (fun c -> match c.Zone with | Province _ -> true | _ -> false)  |> List.map (cardView actions dispatch)))     

let strongholdView (cards: Card list) (actions : ClientPlayerAction list) dispatch = 
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
              cardView actions dispatch stronghold
              cardView actions dispatch strongholdProvince]

let playerView player ps invert (actions : ClientPlayerAction list) dispatch =
    let view = 
      [ 
          div
            []
            [ str <| sprintf "Player: %A" player
              str <| sprintf "Honor: %d" ps.Honor
              str <| sprintf "Fate: %d" ps.Fate
              str <| sprintf "Conflict: %d" ps.ConflictDeckCount
              str <| sprintf "Dynasty: %d" ps.DynastyDeckCount ]
          strongholdView ps.CardsInPlay actions dispatch
          provincesView ps.CardsInPlay actions dispatch
          dynastyZoneView ps.CardsInPlay actions dispatch
          inPlayView ps.CardsInPlay actions dispatch ]
    div 
      []
      (if invert then List.rev view else view)
      
let gameStateView model actions dispatch = 
    div 
      []
      [ str <| sprintf "Turn: %d" model.TurnNumber 
        str <| sprintf "Phase: %A" model.GamePhase
        str <| sprintf "Active: %A" model.ActivePlayer
        playerView Player1 model.Player1ClientState false actions dispatch
        playerView Player2 model.Player2ClientState true actions dispatch]


let view model dispatch =
    div 
        []
        [ button [ OnClick (fun _ -> dispatch NewGame)] [str "New Game"] 
          actionsView model.Actions dispatch
          gameStateView model.State model.Actions dispatch]

//let connection = Debugger.connect (Debugger.Remote("localhost",8000))

Program.mkProgram init update view
|> Program.withReact "elmish-app"
//|> Program.withDebuggerUsing connection
|> Program.run
