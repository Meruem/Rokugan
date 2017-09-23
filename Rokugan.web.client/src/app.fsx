#I "../node_modules"
#r "fable-core/Fable.Core.dll"
#r "fable-elmish/Fable.Elmish.dll"
#r "fable-elmish-react/Fable.Elmish.React.dll"
#r "fable-react/Fable.React.dll"

open Fable.Core
open Fable.React
open Fable.Import.React
open Fable.Helpers.React.Props
open Fable.Helpers.React

open Elmish
open Elmish.React



type Model = { Count : int }

type Messages = Incr | Decr 

let update msg model = 
    match msg with
    | Incr -> { Count = model.Count + 1 }
    | Decr -> { Count = model.Count - 1 }

// Helper function, a string is a valid ReactElement
let text (content: string) : ReactElement = unbox content
// Helper function, for the initial model
let init() = { Count = 0 }

let view model dispatch = 
    div 
      []
      [
          button [ OnClick (fun _ -> dispatch Incr) ] [ text "Increment" ]
          div [] [ text (string model.Count) ]
          button [ OnClick (fun _ -> dispatch Decr) ] [ text "Decrement"]
      ]

Program.mkSimple init update view
|> Program.withReact "elmish-calc"
|> Program.run