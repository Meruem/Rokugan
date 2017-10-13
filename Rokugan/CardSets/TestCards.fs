module TestCards

open RokuganShared
open GameTypes
open CardDef

let ``Generic 1/1`` = 
    title "Generic 1/1" 
    @+> dynastychar 1 (Some 1) (Some 1) 0 []

let ``Generic 2/2`` =
    title "Generic 2/2" 
    @+> dynastychar 2 (Some 2) (Some 2) 0 []
        
let ``Holding`` =
    title  "Holding" 
    @+> holding 1 []

let ``Event`` =
    title "Event"
    @+> event 1
let ``Attachment`` =
    title "Attachment"
    @+> attachment 1 2 0 [Weapon]
let ``Province str 4``=
    title "Province str 4"
    @+> province 4 Earth
let ``Stronghold`` =
    title "Stronghold"
    @+> stronghold 0 10 7 10
let testCards = 
  [ ``Generic 1/1``
    ``Generic 2/2``
    ``Holding``
    ``Event``
    ``Attachment``
    ``Province str 4``
    ``Stronghold``]
