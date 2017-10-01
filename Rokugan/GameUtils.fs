[<AutoOpen>]
module GameUtils

open GameTypes
open RokuganShared

let chooseRandomPlayer () = 
    let rnd = System.Random()
    if rnd.Next(2) = 0 then Player1 else Player2

