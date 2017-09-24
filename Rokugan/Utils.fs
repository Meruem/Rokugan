[<AutoOpen>]
module Utils

open Microsoft.FSharp.Reflection

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let tryFromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None

let fromString<'a> (s:string) = 
    match tryFromString<'a> s with
    | Some x -> x
    | None -> failwithf "unable to convert string %s to type %s" s ((typeof<'a>).ToString())

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

let listSplitBy pred lst =
    List.foldBack (fun item (x,y) -> if pred item then (item::x,y) else (x, item::y)) lst ([],[])

let mutable lastId = 1
let newId () = 
    lastId <- lastId + 1
    lastId

let toValuesList m = m |> Map.toList |> List.map (fun (k, v) -> v)

