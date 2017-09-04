module Conflict

open GameTypes
open GameState
open PlayerState

let availableConflicts ps = 
    if ps.DeclaredConflicts.Length >= 2 then [] 
    else [Military;Political] |> List.filter (fun ct -> not (List.contains (Some ct) ps.DeclaredConflicts))

let availableRings gs = gs.Rings |> List.filter (fun r -> r.State = Unclaimed)

let getCharactersForConflict cType ps = 
    let notBowed c = not (Card.isBowed c)
    ps.Home.Cards |> List.filter (fun c -> notBowed c && Card.isCharWithValue cType c)

let calculateTotalSkill cType chars = 
    chars |> List.sumBy (fun c -> match Card.charSkillValue cType c with | None -> 0 | Some s -> s)

let resolveConflict cType ring (province:Province) attackers defenders getConflictPhaseActions gs = 
    let totalAttack = attackers |> calculateTotalSkill cType
    let totalDefence = defenders |> calculateTotalSkill cType
    // resolve ring effect
    // break province
    // bow combatants
    // break province
    // loose honor if undefended
    gs |> switchActivePlayer |> getConflictPhaseActions

let rec chooseDefenders cType ring (province:Province) attackers defenders getConflictPhaseActions gs =
    let defenderChosen defender = chooseDefenders cType ring province attackers (defender::defenders) getConflictPhaseActions
    let passAction = 
      { Type = Pass
        Action = resolveConflict cType ring province attackers defenders getConflictPhaseActions }
    let actions = 
        getCharactersForConflict cType (activePlayerState gs) 
        |> List.map (fun char -> 
            {Type = ChooseDefender char
             Action = defenderChosen char })
    { gs with Actions = actions }

let rec chooseAttackers cType ring (province:Province) attackers getConflictPhaseActions gs = 
    let attackerChosen attacker = chooseAttackers cType ring province (attacker::attackers) getConflictPhaseActions
    let passAction = 
      { Type = Pass
        Action = chooseDefenders cType ring province attackers [] getConflictPhaseActions }
    let actions = 
        getCharactersForConflict cType (activePlayerState gs) 
        |> List.map (fun char -> 
            {Type = ChooseAttacker char
             Action = attackerChosen char })
    { gs with Actions = actions }

let passConflict gs = 
    let pass ps = {ps with DeclaredConflicts = None :: ps.DeclaredConflicts}
    gs |> changeActivePlayerState pass |> switchActivePlayer 

let attack cType ring gs =
    let changePlState ps = 
      { ps with 
          Fate = ps.Fate + ring.Fate 
          DeclaredConflicts = Some cType :: ps.DeclaredConflicts }
    gs |> removeFateFromRing ring |> changeActivePlayerState changePlState      

let provinceChosen cType ring province gs = 
    gs |> changeOtherPlayerState (revealProvince province)

let chooseProvince cType ring getConflictPhaseActions gs = 
    let ps = gs |> otherPlayerState
    let provinces = ps.Provinces |> List.filter (fun p -> p.State <> Broken)
    let provinces' = if provinces.Length <= 1 then ps.StrongholdProvince :: provinces else provinces
    let actions = 
        provinces' 
        |> List.map (fun p -> 
            { Type = ChooseProvince p
              Action = provinceChosen cType ring p >> attack cType ring >> chooseAttackers cType ring p [] getConflictPhaseActions})
    { gs with Actions = actions }


let rec getConflictPhaseActions gs =
    let ps = GameState.activePlayerState gs
    let createDeclareConflictAction cType ring = 
      { Type = DeclareAttack (cType,ring.Element)
        Action = chooseProvince cType ring getConflictPhaseActions}
    let passAction = 
        { Type = Pass
          Action = passConflict >> getConflictPhaseActions}
    let actions = 
        [for ct in (availableConflicts ps) do
            for ring in (availableRings gs) do yield createDeclareConflictAction ct ring] 
    { gs with Actions = actions} 
  