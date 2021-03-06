module Deck

open GameTypes
open System

let drawCardFromDeck (deck : Deck) =
    let (Deck lst) = deck
    match lst with
    | card :: rest -> Some (card, Deck rest)
    | _ -> None

let shuffleDeck (deck:Deck) =
    deck.Cards |> List.sortBy (fun c -> Guid.NewGuid ()) |> Deck

let getCardsFromDeck n (deck:Deck) = 
    let (Deck lst) = deck
    let hand, rest = List.splitAt n lst
    hand, (Deck rest)    