module Utils

open GameTypes

let chooseRandomPlayer () = 
    let rnd = System.Random()
    if rnd.Next(2) = 0 then Player1 else Player2

let replaceListElement newele pos lst =
    let (_, lst') = 
        lst 
        |> List.fold (fun (i, acc) d -> 
            let c = if i = pos then newele else d
            (i+1, List.append acc [c])) (0, [])
    lst'


