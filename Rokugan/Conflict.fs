module Conflict

open GameTypes
open GameState
open PlayerState
open Actions

type private AttackState =
  { Type : ConflictType
    Attacker : Player
    Ring : Ring
    Province : Card
    Attackers : Card list
    Defenders : Card list }
    with
        member this.Defender = otherPlayer this.Attacker

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


let resolveRingEffect ring next yesNo (gs:GameState) = 
    match yesNo with 
    | No -> gs
    | Yes -> 
        match ring.Element with
        | Element.Fire -> gs |> Ring.resolveFireRing next
        | Element.Air -> gs |> Ring.resolveAirRing next
        | Element.Earth -> gs |> Ring.resolveEarthRing next
        | Element.Void -> gs |> Ring.resolveVoidRing next
        | Element.Water -> gs |> Ring.resolveWaterRing next

let askToResolveRingEffect ring next gs = 
    gs >!=> yesNo "Resolve ring effect" (resolveRingEffect ring next)
    

let private resolveConflict state next gs = 
    let totalAttack = state.Attackers |> calculateTotalSkill state.Type
    let totalDefence = state.Defenders |> calculateTotalSkill state.Type
    let attackerWon = totalAttack >= totalDefence && totalAttack > 0
    let defenderWon = totalDefence > totalAttack || totalAttack = 0 

    let claimRing gs =
        if attackerWon then 
            gs |> Ring.claimRing state.Ring state.Attacker
        else if totalDefence > 0 then
            gs |> Ring.claimRing state.Ring state.Defender
            else gs |> Ring.returnRing state.Ring

    let looseHonorIfUndefended gs =
        if state.Defenders.Length = 0 then gs |> changeOtherPlayerState (addHonor -1)
        else gs

    let bowCombatants gs =
        gs 
        |> changeCards Card.bow (List.append state.Attackers state.Defenders)
   
    let breakProvince gs = 
        let provDef = CardRepository.getProvinceCard state.Province.Title
        if totalAttack >= totalDefence + provDef.Strength then gs |> changeCard Card.breakProvince state.Province
        else gs 

    let afterRings = 
        claimRing
        >> breakProvince 
        >> bowCombatants 
        >> looseHonorIfUndefended 
        >> switchActivePlayer 
        >> next
    if attackerWon then askToResolveRingEffect state.Ring afterRings gs else afterRings gs
 

let rec private chooseDefenders state next (gs:GameState) =
    let defenderChosen defender = chooseDefenders {state with Defenders = defender :: state.Defenders} next
    let passAction = pass gs.ActivePlayer (switchActivePlayer >> resolveConflict state next)
    let actions = 
        getCharactersForConflict state.Type state.Defenders gs.ActivePlayerState 
        |> List.map (fun char -> action (ChooseDefender char) (defenderChosen char))
    gs >!=> [passAction] >+=> actions

let rec private chooseAttackers state next (gs:GameState) = 
    let attackerChosen attacker = chooseAttackers {state with Attackers = attacker :: state.Attackers } next
    let passAction = pass gs.ActivePlayer (switchActivePlayer >> chooseDefenders state next)
    let actions = 
        getCharactersForConflict state.Type state.Attackers gs.ActivePlayerState 
        |> List.map (fun char -> action (ChooseAttacker char)(attackerChosen char))
    gs >!=> [passAction] >+=> actions

let passConflict gs = 
    let pass ps = {ps with DeclaredConflicts = None :: ps.DeclaredConflicts}
    gs |> changeActivePlayerState pass |> switchActivePlayer 

let attack cType ring gs =
    let changePlState (ps:PlayerState) = 
      { ps with 
          Fate = ps.Fate + ring.Fate 
          DeclaredConflicts = Some cType :: ps.DeclaredConflicts }
    gs |> removeFateFromRing ring |> changeActivePlayerState changePlState      

let provinceChosen cType ring province gs = 
    gs |> changeOtherPlayerState (revealProvince province)

let chooseProvince cType ring next gs = 
    let ps = gs |> otherPlayerState
    let provinces = ps.Provinces |> List.filter (Card.isProvinceBroken >> not)
    let provinces' = if provinces.Length <= 1 then ps.StrongholdProvince :: provinces else provinces
    let actions = 
        provinces' 
        |> List.map (fun p -> 
            chooseProvince p 
                (provinceChosen cType ring p 
                    >> attack cType ring 
                    >> chooseAttackers 
                        {Type = cType
                         Attacker = gs.ActivePlayer
                         Ring = ring
                         Province = p
                         Attackers = []
                         Defenders = []} next ))
    gs >!=> actions


let rec addConflictActions (gs:GameState) =
    let ps = gs.ActivePlayerState
    let createDeclareConflictAction cType ring = 
        declareAttack 
            cType ring.Element
            (chooseProvince cType ring addConflictActions)
    let passAction = pass gs.ActivePlayer (passConflict >> addConflictActions)
    let actions = 
        [for ct in (availableConflicts ps) do
            for ring in (availableRings gs) do yield createDeclareConflictAction ct ring] 
    gs >!=> [passAction] >+=> actions
  
let gotoConflictPhase (gs:GameState) = 
    { gs with
        GamePhase = Conflict
        ActivePlayer = gs.FirstPlayer }
    |> addConflictActions

let cleanDeclaredConflicts gs =
    let clearConflict ps = {ps with DeclaredConflicts = []}
    gs 
    |> changePlayerState Player1 clearConflict
    |> changePlayerState Player2 clearConflict