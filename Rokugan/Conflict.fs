module Conflict

open GameTypes
open GameState
open PlayerState
open Actions
open CardRepository

let attackState gs = 
    match gs.AttackState with
    | Some st -> st
    | None -> failwith "should not happen"

let availableConflicts ps = 
    if ps.DeclaredConflicts.Length >= 2 then [] 
    else [Military;Political] |> List.filter (fun ct -> not (List.contains (Some ct) ps.DeclaredConflicts))

let availableRings gs = gs.Rings |> List.filter (fun r -> r.State = Unclaimed)

let getCharactersForConflict cType alreadyInConflict (ps:PlayerState) = 
    let notBowed c = not (Card.isBowed c)
    ps.Home 
        |> List.filter (fun c -> notBowed c && Card.isCharWithValue cType c && not (List.contains c alreadyInConflict))

let calculateTotalSkill cType chars = 
    chars 
    |> List.sumBy (fun c -> if Card.isBowed c then 0 else match Card.charSkillValue cType c with | None -> 0 | Some s -> s)

let resolveRingEffect (gs:GameState) yesNo =
    let state = attackState gs
    let ring = state.Ring
    match yesNo with 
    | No -> none
    | Yes -> 
        match ring.Element with
        | Element.Fire -> Ring.resolveFireRing state.Attacker 
        | Element.Air -> Ring.resolveAirRing state.Attacker 
        | Element.Earth -> Ring.resolveEarthRing state.Attacker 
        | Element.Void -> Ring.resolveVoidRing state.Attacker 
        | Element.Water ->  Ring.resolveWaterRing state.Attacker 
    

let getConflictResolveActions gs =
    let state = attackState gs

    let totalAttack = state.Attackers |> calculateTotalSkill state.Type
    let totalDefence = state.Defenders |> calculateTotalSkill state.Type
    let attackerWon = totalAttack >= totalDefence && totalAttack > 0
    let defenderWon = totalDefence > totalAttack || totalAttack = 0     

    let looseHonorIfUndefended =
        if state.Defenders.Length = 0 then [AddHonor (state.Defender, -1)] else []

    let bowCombatants =
        state.Attackers @ state.Defenders |> List.map Bow

    let breakProvince = 
        let provDef = repository.GetProvinceCard state.Province.Title
        if totalAttack >= totalDefence + provDef.Strength then [BreakProvince state.Province] else []

    if attackerWon then 
        changes  
            [SwitchActivePlayer
             ClaimRing (state.Attacker, state.Ring)]
        >+> playerActions (fun gs -> yesNo state.Attacker "Resolve ring effect" (resolveRingEffect gs))
    else 
        changes
            ([SwitchActivePlayer]
            @ if defenderWon then [ClaimRing (state.Defender, state.Ring)] else [ReturnRing state.Ring])
    >+>
        changes 
            (looseHonorIfUndefended
            @ bowCombatants
            @ breakProvince
            @ [SwitchActivePlayer])



let rec private chooseDefenders (gs:GameState) =
    let state = attackState gs
    let passAction = 
        pass 
            state.Defender 
            (getConflictResolveActions gs)

    let actions = 
        getCharactersForConflict state.Type state.Defenders gs.ActivePlayerState 
        |> List.map (fun char -> action 
                                    state.Defender 
                                    (ChooseDefender char) 
                                    (changes [DeclareDefender char]
                                    >+> playerActions chooseDefenders))
    [passAction] @ actions

let passConflict gs = 
    [PassConflict gs.ActivePlayer
     SwitchActivePlayer]

let rec private chooseAttackers (gs:GameState) = 
    let state = attackState gs
    let passAction = 
        if state.Attackers.Length = 0 then
            pass gs.ActivePlayer (changes (passConflict gs))
        else
            pass 
                gs.ActivePlayer
                (changes
                    [SwitchActivePlayer
                     ConflictStarted
                     ContestRing state.Ring]
                >+> playerActions chooseDefenders)
    let actions = 
        getCharactersForConflict state.Type state.Attackers gs.ActivePlayerState 
        |> List.map (fun char -> action 
                                    state.Attacker 
                                    (ChooseAttacker char)
                                    (changes [DeclareAttacker char]
                                    >+> playerActions chooseAttackers))
    [passAction] @ actions
 

let availableProvinces player gs =
    let ps = gs |> playerState player
    let provinces = ps.Provinces |> List.filter (Card.isProvinceBroken >> not)
    if provinces.Length <= 1 then ps.StrongholdProvince :: provinces else provinces

let revealProvince card = if Card.isHidden card then [RevealProvince card] else []

let rec conflictActions (gs:GameState) =
    let ps = gs.ActivePlayerState
    let createDeclareConflictAction ct ring prov = 
        declareAttack 
            gs.ActivePlayer ct ring prov
            (changes 
                ([DeclareConflict (gs.ActivePlayer, ct, ring, prov) ]
                @ revealProvince prov)
            >+> playerActions chooseAttackers
            >+> playerActions conflictActions)
    let passAction = 
        let cnt = 
            if gs.OtherPlayerState.DeclaredConflicts.Length = 2 then 
                changes (passConflict gs) 
            else changes (passConflict gs) >+> playerActions conflictActions
        pass gs.ActivePlayer cnt
    let actions = 
        [for ct in (availableConflicts ps) do
            for ring in (availableRings gs) do 
                for prov in (availableProvinces gs.OtherPlayer gs) do yield createDeclareConflictAction ct ring prov] 
    [passAction] @ actions
  
let gotoConflictPhase nextPhase gs =
    change (ChangePhase Conflict)
    >+> playerActions conflictActions
    >+!> nextPhase


// -------------------------- message handlers ----------------------------      

let onPassConflict player gs = 
    let pass ps = {ps with DeclaredConflicts = None :: ps.DeclaredConflicts}
    gs |> changePlayerState player pass  

let onConflictDeclared player ctype ring province gs =
    { gs with 
        AttackState =
          Some
              { Attacker = gs.ActivePlayer
                Type = ctype
                Ring = ring
                Province = province
                Attackers = []
                Defenders = []}}
 
let onAttackerDeclared card gs = 
    let state = attackState gs
    let state' = {state with Attackers = card :: state.Attackers}
    {gs with AttackState = Some state' }

let onDefenderDeclared card gs = 
    let state = attackState gs
    let state' = {state with Defenders = card :: state.Defenders}
    {gs with AttackState = Some state' }    

let onConflictStarted gs = 
    let state = attackState gs
    gs |> changePlayerState state.Attacker (fun st -> {st with DeclaredConflicts = Some state.Type :: st.DeclaredConflicts}) 