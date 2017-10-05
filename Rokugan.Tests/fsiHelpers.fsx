open RokuganShared
open GameTypes

let titleStr (Title title) = title 
//let idStr (CardId id) = id
let idStr id = id
let cardStr c = sprintf "Card %i [%s] in %A (+%i)" (idStr c.Id) (titleStr c.Title) c.Zone c.Fate

fsi.AddPrintTransformer (titleStr >> box)
//fsi.AddPrintTransformer (idStr >> box)
//fsi.AddPrintTransformer (fun (a:PlayerAction) -> box (sprintf "Action(%A): %A" a.Player a.Type))
//fsi.AddPrinter (fun (t:GameTrigger) -> sprintf "Trigger %s [%A]" t.Name t.Lifetime)

fsi.AddPrinter (fun (d : Deck) -> 
    match d.Cards with
    | c :: _ -> sprintf "Deck: %i cards, top: [%s]" d.Cards.Length (cardStr c)
    | [] -> sprintf "Deck: empty")
fsi.AddPrinter (cardStr)
//fsi.AddPrinter (fun (r:Ring) -> sprintf "%A/%A/(%i)" r.Element r.State r.Fate)