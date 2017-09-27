[<AutoOpen>]
module Transform

open GameTypes

let action player actionType transform = { Type = actionType; Player = player; OnExecute = transform}

// creates multiple choice action from min to max
let choicei player desc min max getTransform =
    [min..max]
        |> List.map (fun i -> action player (Choicei (i, desc)) (getTransform i))

let choice player desc xs getTransform = 
    xs |> List.map (fun s -> action player (Choice (s, desc)) (getTransform s))

let yesNo player desc getTransform = 
    let yesCont = getTransform Yes
    let noCont = getTransform No
    [ action player (YesNoChoice (Yes, desc)) (getTransform Yes)
      action player (YesNoChoice (No, desc)) (getTransform No)]


let transform commands next cont = {Commands = commands; NextActions = next; Continuation = cont}

let none = transform [] None []

let playerActionsM actions = transform [] actions []
let playerActions actions = playerActionsM (Some actions)

let changes commands = transform commands None []
let change command = changes [command]

let rec reduce tr = 
    if tr.NextActions.IsNone then
        match tr.Continuation with
        | x :: xs  -> 
            let cont = x ()
            let reduced = transform (tr.Commands @ cont.Commands) cont.NextActions (cont.Continuation @ xs)
            reduce reduced
        | [] -> tr
    else tr

let rec addContinuation t cont =
    match t.Continuation with
    | [] -> {t with Continuation = [cont]}
    | _ -> {t with Continuation = t.Continuation @ [cont]}

let addTransforms t1 t2 = 
    let t1' = reduce t1
    let t2' = reduce t2
    if List.isEmpty t2'.Commands && List.isEmpty t2'.Continuation && t2'.NextActions.IsNone then t1' else
        match t1'.NextActions with
        | None -> {t2' with Commands = t1'.Commands @ t2'.Commands}
        | Some _ -> addContinuation t1' (fun () -> t2')

let addTransformsLazy t1 t2 = 
    let t1' = reduce t1
    match t1'.NextActions with
    | None -> 
        let t2' = t2() 
        {t2' with Commands = t1'.Commands @ t2'.Commands}
    | Some _ -> addContinuation t1' t2
let (>+>) = addTransforms
let (>+!>) = addTransformsLazy
