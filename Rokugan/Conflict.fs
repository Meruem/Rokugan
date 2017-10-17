module Conflict

open RokuganShared
open GameTypes
open GameState
open PlayerState
open PlayerActions
open CardRepository
open CardDef

let attackState gs = 
    match gs.AttackState with
    | Some st -> st
    | None -> failwith "should not happen"

let availableConflicts ps = 
    if ps.DeclaredConflicts.Length >= 2 then [] 
    else [Military;Political] |> List.filter (fun ct -> not (List.contains (Some ct) ps.DeclaredConflicts))

let availableRings gs = gs.Rings |> List.filter (fun r -> r.State = Unclaimed)

let getCharactersForConflict attackOrDefence cType alreadyInConflict player (gs:GameState) = 
    let ps = gs |> playerState player
    let notBowed c = not (Card.isBowed c)
    let efType = match attackOrDefence with | Attack -> CannotAttack | Defence -> CannotBlock
    let cannotBlock c = gs.CardEffects |> List.exists (fun ce -> ce.Card = c && ce.Type = efType)
    ps.Home 
        |> List.filter (fun c -> notBowed c && Card.isCharWithValue cType c && not (List.contains c alreadyInConflict))

let calculateTotalSkill cType chars = 
    chars 
    |> List.sumBy (fun c -> if Card.isBowed c then 0 else match Card.charSkillValue cType c with | None -> 0 | Some s -> s)

let resolveRingEffect (gs:GameState) yesNo =
    let state = attackState gs
    let ring = state.Ring
    match yesNo with 
    | No -> none ()
    | Yes -> 
        match ring.Element with
        | Element.Fire -> Ring.resolveFireRing state.Attacker 
        | Element.Air -> Ring.resolveAirRing state.Attacker 
        | Element.Earth -> Ring.resolveEarthRing state.Attacker 
        | Element.Void -> Ring.resolveVoidRing state.Attacker 
        | Element.Water ->  Ring.resolveWaterRing state.Attacker 
        | _ -> failwith "ring should always have defined element"
    
let endConflict gs =
    let state = attackState gs
    let winners = 
        if attackerWon gs then state.Attackers 
        else if defenderWon gs then state.Defenders else []
    let loosers = 
        if attackerWon gs then state.Defenders 
        else if defenderWon gs then state.Attackers else []
    [ConflictEnd (Some {Winners = winners; Loosers = loosers}) ])

let getConflictResolveActions =
    let totalAttack gs = (attackState gs).Attackers |> calculateTotalSkill (attackState gs).Type
    let totalDefence gs = (attackState gs).Defenders |> calculateTotalSkill (attackState gs).Type
    let attackerWon gs = (totalAttack gs) >= (totalDefence gs) && (totalAttack gs) > 0
    let defenderWon gs = (totalDefence gs) > (totalAttack gs) || (totalAttack gs) = 0     
    // rewrite! eyes bleeding

    let looseHonorIfUndefended gs =
        if (attackState gs).Defenders.Length = 0 then [AddHonor ((attackState gs).Defender, -1)] else []

    let bowCombatants gs =
        (attackState gs).Attackers @ (attackState gs).Defenders |> List.map Bow

    let breakProvince gs = 
        let provDef = repository.GetProvinceCard (attackState gs).Province.Title
        if  (totalAttack gs) >= (totalDefence gs) + provDef.Strength then [BreakProvince (attackState gs).Province] else []

    Actions.actionWindow Defender "Conflict actions: "
    >+> act looseHonorIfUndefended
    >+> act breakProvince
    >+> act (fun gs -> 
        if attackerWon gs then 
                [SwitchActivePlayer
                 ClaimRing ((attackState gs).Attacker, (attackState gs).Ring)]
        else 
                ([SwitchActivePlayer]
                @ if (defenderWon gs) then [ClaimRing ((attackState gs).Defender, (attackState gs).Ring)] else [ReturnRing (attackState gs).Ring]))
    >+> playerActions 
            (fun gs ->
                if (attackerWon gs) then (yesNo (attackState gs).Attacker "Resolve ring effect" (resolveRingEffect gs)) else [])
            "Resolve ring effect: "
    >+> act bowCombatants
    >+> act endConflict
    >+> act (fun gs -> [SetActivePlayer (attackState gs).Defender])


let rec private chooseDefenders (gs:GameState) =
    let state = attackState gs
    let passAction = 
        pass 
            state.Defender 
            getConflictResolveActions

    let actions = 
        getCharactersForConflict Defence state.Type state.Defenders state.Defender gs
        |> List.map (fun char -> action 
                                    state.Defender 
                                    (ChooseDefender char) 
                                    (changes [DeclareDefender char]
                                    >+> playerActions chooseDefenders "Choose defenders: "))
    [passAction] @ actions


let passConflict gs = 
    [PassConflict gs.ActivePlayer
     ConflictEnd None
     SwitchActivePlayer]

let rec private chooseAttackers (gs:GameState) = 
    let state = attackState gs
    let passAction = 
        if state.Attackers.Length = 0 then
            pass gs.ActivePlayer (act passConflict)
        else
            pass 
                gs.ActivePlayer
                (changes
                    [SwitchActivePlayer
                     ConflictStarted
                     ContestRing state.Ring]
                >+> playerActions chooseDefenders "Choose defenders: ")
    let actions = 
        getCharactersForConflict Attack state.Type state.Attackers state.Attacker gs
        |> List.map (fun char -> action 
                                    state.Attacker 
                                    (ChooseAttacker char)
                                    (changes [DeclareAttacker char]
                                    >+> playerActions chooseAttackers "Choose attackers: "))
    [passAction] @ actions
 

let availableProvinces player gs =
    let ps = gs |> playerState player
    let provinces = ps.Provinces |> List.filter (Card.isProvinceBroken >> not)
    if provinces.Length <= 1 then ps.StrongholdProvince :: provinces else provinces

let revealProvince card = if Card.isHidden card then [RevealProvince card] else []

let rec declareAttackActions (gs:GameState) =
    let ps = gs.ActivePlayerState
    let createDeclareConflictAction ct ring prov = 
        declareAttack 
            gs.ActivePlayer ct ring prov
            (changes 
                ([DeclareConflict (gs.ActivePlayer, ct, ring, prov) ]
                @ revealProvince prov)
            >+> playerActions chooseAttackers "Choose attackers: "
            >+> Actions.actionWindow FirstPlayer "Pre-conflict: "
            >+> playerActions declareAttackActions "Declare conflict: ")
    let passAction = 
        let cnt = 
            if gs.OtherPlayerState.DeclaredConflicts.Length = 2 then 
                changes [PassConflict gs.ActivePlayer; ConflictEnd None] 
            else 
                changes [PassConflict gs.ActivePlayer; ConflictEnd None] 
                >+> Actions.actionWindow FirstPlayer "Pre-conflict: "
                >+> changes [SetActivePlayer (otherPlayer gs.ActivePlayer)]
                >+> playerActions declareAttackActions "Declare conflict: "
        pass gs.ActivePlayer cnt
    let actions = 
        [for ct in (availableConflicts ps) do
            for ring in (availableRings gs) do 
                for prov in (availableProvinces gs.OtherPlayer gs) do yield createDeclareConflictAction ct ring prov] 
    [passAction] @ actions
  
let gotoConflictPhase nextPhase =
    change (ChangePhase GamePhase.Conflict)
    >+> changes [SetFirstPlayerActive]
    >+> Actions.actionWindow FirstPlayer "Pre-conflict: "
    >+> playerActions declareAttackActions  "Declare conflict: "
    >+!> nextPhase


// -------------------------- message handlers ----------------------------      

let onPassConflict player gs = 
    let pass (ps:PlayerState) = {ps with DeclaredConflicts = None :: ps.DeclaredConflicts}
    gs |> changePlayerState player pass  

let onConflictDeclared player ctype ring province (gs:GameState) =
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

let onConflictEnd state = 
    cleanEffectsByLifetime Lifetime.Conflict
