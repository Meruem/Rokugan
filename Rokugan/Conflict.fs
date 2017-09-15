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
    chars |> List.sumBy (fun c -> if Card.isBowed c then 0 else match Card.charSkillValue cType c with | None -> 0 | Some s -> s)

let resolveRingEffect cont (gs:GameState) yesNo =
    let state = attackState gs
    let ring = state.Ring
    match yesNo with 
    | No -> cont
    | Yes -> 
        match ring.Element with
        | Element.Fire -> Ring.resolveFireRing state.Attacker cont
        | Element.Air -> Ring.resolveAirRing state.Attacker cont
        | Element.Earth -> Ring.resolveEarthRing state.Attacker cont
        | Element.Void -> Ring.resolveVoidRing state.Attacker cont
        | Element.Water ->  Ring.resolveWaterRing state.Attacker cont
    

let getConflictResolveActions getConflictActions gs =
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

    let cont = 
        { Commands =
            looseHonorIfUndefended
            @ bowCombatants
            @ breakProvince
            @ [SwitchActivePlayer]
          NextActions = getConflictActions}
    let trans = resolveRingEffect cont gs
    if attackerWon then 
        { Commands = [SwitchActivePlayer]
          NextActions = fun _ -> yesNo state.Attacker "Resolve ring effect" trans }
    else  
        [SwitchActivePlayer] @+ cont


let rec private chooseDefenders getConflictActions (gs:GameState) =
    let state = attackState gs
    let passAction = 
        pass 
            state.Defender 
            (getConflictResolveActions getConflictActions gs)

    let actions = 
        getCharactersForConflict state.Type state.Defenders gs.ActivePlayerState 
        |> List.map (fun char -> action 
                                    state.Defender 
                                    (ChooseDefender char) 
                                    { Commands = [DeclareDefender char]
                                      NextActions = chooseDefenders getConflictActions })
    [passAction] @ actions

let passConflict gs = 
    [PassConflict gs.ActivePlayer
     SwitchActivePlayer]

let rec private chooseAttackers getConflictActions (gs:GameState) = 
    let state = attackState gs
    let passAction = 
        if state.Attackers.Length = 0 then
            pass gs.ActivePlayer { Commands = passConflict gs; NextActions = getConflictActions }
        else
            pass 
                gs.ActivePlayer
                { Commands =
                    [SwitchActivePlayer
                     ConflictStarted]
                  NextActions = chooseDefenders getConflictActions }
    let actions = 
        getCharactersForConflict state.Type state.Attackers gs.ActivePlayerState 
        |> List.map (fun char -> action 
                                    state.Attacker 
                                    (ChooseAttacker char)
                                    { Commands = [DeclareAttacker char]
                                      NextActions = chooseAttackers getConflictActions })
    [passAction] @ actions
 

let availableProvinces player gs =
    let ps = gs |> playerState player
    let provinces = ps.Provinces |> List.filter (Card.isProvinceBroken >> not)
    if provinces.Length <= 1 then ps.StrongholdProvince :: provinces else provinces

let revealProvince card = if Card.isHidden card then [RevealProvince card] else []

let rec conflictActions (nextPhase:Transform) (gs:GameState) =
    let ps = gs.ActivePlayerState
    let createDeclareConflictAction ct ring prov = 
        declareAttack 
            gs.ActivePlayer ct ring prov
            { Commands =
                ([DeclareConflict (gs.ActivePlayer, ct, ring, prov) ]
                @ revealProvince prov)
              NextActions = chooseAttackers (conflictActions nextPhase) }
    let passAction = pass gs.ActivePlayer { Commands = (passConflict gs); NextActions = conflictActions nextPhase }
    let actions = 
        [for ct in (availableConflicts ps) do
            for ring in (availableRings gs) do 
                for prov in (availableProvinces gs.OtherPlayer gs) do yield createDeclareConflictAction ct ring prov] 
    [passAction] @ actions
  
let gotoConflictPhase =
    [ChangePhase Conflict]

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