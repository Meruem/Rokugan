module CoreCards

open RokuganShared
open GameTypes
open CardDef

let ``Akodo Gunsō`` = 
    title "Akodo Gunsō" 
    @+> clan Clan.Lion
    @+> dynastychar 2 (Some 2)  (Some 1) 2  [Bushi]
    @+> action  { Name = "Some action"
                  Condition = fun card gs -> true
                  Effect = fun card -> changes [Debug (sprintf "Activated card %A %d" card.Title card.Id )] }
    @+> trigger { Name = "covert"
                  Lifetime = Game
                  Condition = fun card cmd state ->
                    match cmd with
                    | DeclareAttacker c -> c.Id = card.Id 
                    | _ -> false
                  Transform = fun card -> 
                        changes [Debug "covert activated"]
                        >+> covert card.Owner }             

let ``Adept of the Waves`` =
    title "Adept of the Waves" 
    @+> clan Clan.Phoenix
    @+> dynastychar 2 (Some 2) (Some 2) 2 [Shugenja; Water]
    @+> trigger { Name = "covert"
                  Lifetime = Game
                  Condition = fun card cmd state ->
                    match cmd with
                    | DeclareAttacker c -> c.Id = card.Id 
                    | _ -> false
                  Transform = fun card -> 
                        changes [Debug "covert activated"]
                        >+> covert card.Owner } 
    
let ``Artisan Academy`` =
    title "Artisan Academy"
    @+> clan Clan.Crane
    @+> holding 1 [Academy]

let ``Admit Defeat`` =
    title "Admit Defeat"
    @+> clan Clan.Crane
    @+> event 1

let ``Ancestral Daishō`` = 
    title "Ancestral Daishō"
    @+> clan Clan.Dragon
    @+> attachment 1 2 0 [Weapon]

let ``Defend the Wall`` = 
    title "Defend the Wall"
    @+> clan Clan.Crab
    @+> province 4 Earth

let ``Golden Plains Outpost`` =
    title "Golden Plains Outpost"
    @+> clan Clan.Unicorn
    @+> stronghold 0 10 7 10

let addCore def = def @+> cardset Core

let coreCards =
   [ ``Akodo Gunsō`` 
     ``Adept of the Waves``
     ``Artisan Academy``
     ``Admit Defeat``
     ``Ancestral Daishō``
     ``Defend the Wall``
     ``Golden Plains Outpost`` ]
   |> List.map addCore
