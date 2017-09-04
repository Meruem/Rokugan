module Utils

open GameTypes

let chooseRandomPlayer () = 
    let rnd = System.Random()
    if rnd.Next(2) = 0 then Player1 else Player2

let replaceListElementi newele pos lst =
    let (_, lst') = 
        lst 
        |> List.fold (fun (i, acc) d -> 
            let c = if i = pos then newele else d
            (i+1, List.append acc [c])) (0, [])
    lst'

let replaceListElement newele pred lst =
    lst 
    |> List.fold (fun acc d -> 
        let c = if pred d then newele else d
        List.append acc [c]) []

let mutable lastId = 1
let newId () = 
    lastId <- lastId + 1
    lastId

