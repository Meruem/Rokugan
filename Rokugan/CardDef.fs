namespace CardDef

open RokuganShared
open GameTypes

// ------------------------ Card definitions ---------------------

type CharacterCardDef = {
    Cost : int
    MilitarySkill : int option
    PoliticalSkill : int option
    Glory : int
    Traits : Trait list }

type HoldingCardDef = {
    BonusStrength : int     
    Traits : Trait list }

type DynastyCardDef = 
    | Character of CharacterCardDef
    | Holding of HoldingCardDef

type EventCardDef = {
    Cost : int }

type AttachmentCardDef = {
    Cost : int
    BonusMilitary : int 
    BonusPolitical : int 
    Traits : Trait list }

type StrongholdCardDef = {
    BonusStrength : int
    StartingHonor : int
    FatePerRound : int
    Influence : int }


type ConflictCardDef = 
    | Character of CharacterCardDef
    | Event of EventCardDef
    | Attachment of AttachmentCardDef

type ProvinceCardDef = {
    Strength : int
    Element : Element }

type RoleCardDef = {
    Abilities : AbilityDef list
    Traits : Trait list }

type CardSpec = 
    | Dynasty of DynastyCardDef
    | Conflict of ConflictCardDef
    | Stronghold of StrongholdCardDef
    | Province of ProvinceCardDef
    | Role of RoleCardDef
    | Undefined

type CardTriggerDef =
      { Name : string
        Lifetime : Lifetime
        Condition : CardId -> Command<GameState,PlayerActionType> -> GameState -> bool
        Effect : CardId -> Command<GameState,PlayerActionType> -> Transform<GameState, Command<GameState,PlayerActionType>, PlayerActionType>}

type CardDef = {
    Title : CardTitle
    Clan : Clan
    Spec : CardSpec 
    Set : CardSet
    Actions : CardActionDef list 
    Triggers : CardTriggerDef list }
        
// -------------------------------------------------------------------------------


module CardDef =
    let dynastyCards (cardList : CardDef list) =
        [for card in cardList do match card.Spec with | CardSpec.Dynasty d -> yield d | _ -> ()]

    let filterConflictCards = List.filter (fun card -> match card.Spec with | CardSpec.Conflict c -> true | _ -> false)
    let filterDynastyCards = List.filter (fun card -> match card.Spec with | CardSpec.Dynasty c -> true | _ -> false)
    let filterStrongholdCards = List.filter (fun card -> match card.Spec with | CardSpec.Stronghold c -> true | _ -> false)
    let filterProvinceCards = List.filter (fun card -> match card.Spec with | CardSpec.Province c -> true | _ -> false)

    let conflictCards (cardList : CardDef list) =
        [for card in cardList do match card.Spec with | CardSpec.Conflict c -> yield c | _ -> ()]
    let strongholdCards (cardList : CardDef list) =
        [for card in cardList do match card.Spec with | CardSpec.Stronghold c -> yield c | _ -> ()]    
    let provinceCards (cardList : CardDef list) =
        [for card in cardList do match card.Spec with | CardSpec.Province c -> yield c | _ -> ()]   

    let actions cardDef = 
        cardDef.Abilities 
        |> List.choose (fun a -> match a with | Action act -> Some act | _ -> None)


    let card title spec clan set actions triggers = 
        {   Title = title
            Spec = spec
            Clan = clan
            Set = set
            Actions = actions
            Triggers = triggers } 

    let char cost military political glory traits =
      { Cost = cost
        MilitarySkill = military
        PoliticalSkill = political
        Glory = glory
        Traits = traits }

    let holdingSpec bonusStrength traits =
        CardSpec.Dynasty (DynastyCardDef.Holding
          { BonusStrength = bonusStrength     
            Traits = traits })

    let eventSpec cost =
        CardSpec.Conflict (ConflictCardDef.Event { EventCardDef.Cost = cost })


    let attachmentSpec cost bonusMilitary bonusPolitical traits=
        CardSpec.Conflict (ConflictCardDef.Attachment 
            { AttachmentCardDef.Cost = cost
              AttachmentCardDef.BonusMilitary = bonusMilitary
              AttachmentCardDef.BonusPolitical = bonusPolitical
              AttachmentCardDef.Traits = traits})    

    let strongholdSpec bonusStrength startingHonor fatePerRound influence =
        CardSpec.Stronghold
          { BonusStrength = bonusStrength
            StartingHonor = startingHonor
            FatePerRound = fatePerRound
            Influence = influence }

    let provinceSpec strength element = 
        CardSpec.Province 
          { Strength = strength
            Element = element }

    let dynastycharSpec cost military political glory traits =  CardSpec.Dynasty (DynastyCardDef.Character (char cost military political glory traits))
    let conflictcharSpec cost military political glory traits = CardSpec.Conflict (ConflictCardDef.Character (char cost military political glory traits))
    let addHoldingSpec (spec1:HoldingCardDef) (spec2:HoldingCardDef) =
        holdingSpec
            (if spec2.BonusStrength <> 0 then spec2.BonusStrength else spec1.BonusStrength)
            (spec1.Traits @ spec2.Traits)

    let addCharacterSpec (spec1:CharacterCardDef) (spec2:CharacterCardDef) = 
        char
            (if spec2.Cost <> 0 then spec2.Cost else spec1.Cost)
            (if spec2.MilitarySkill.IsSome then spec2.MilitarySkill else spec1.MilitarySkill)
            (if spec2.PoliticalSkill.IsSome then spec2.PoliticalSkill else spec1.PoliticalSkill)
            (if spec2.Glory <> 0 then spec2.Glory else spec1.Glory)
            (spec1.Traits @ spec2.Traits)

    let addDynastySpec spec1 spec2 =
        match spec1, spec2 with
        | DynastyCardDef.Character a, DynastyCardDef.Character b ->  CardSpec.Dynasty (DynastyCardDef.Character (addCharacterSpec a b))
        | DynastyCardDef.Holding a, DynastyCardDef.Holding b -> addHoldingSpec a b
        | _ -> failwith "Cannot combine two different types of spec"

    let addEventSpec (spec1: EventCardDef) (spec2:EventCardDef) =
        eventSpec (if spec2.Cost <> 0 then spec2.Cost else spec1.Cost)

    let addAttachmentSpec (spec1: AttachmentCardDef) (spec2:AttachmentCardDef) =
        attachmentSpec
            (if spec2.Cost <> 0 then spec2.Cost else spec1.Cost)
            (if spec2.BonusMilitary <> 0 then spec2.BonusMilitary else spec1.BonusMilitary)
            (if spec2.BonusPolitical <> 0 then spec2.BonusPolitical else spec1.BonusPolitical)
            (spec1.Traits @ spec2.Traits)

    let addConflictSpec spec1 spec2 =
        match spec1, spec2 with
        | ConflictCardDef.Character a, ConflictCardDef.Character b -> CardSpec.Conflict (ConflictCardDef.Character (addCharacterSpec a b))
        | ConflictCardDef.Event a, ConflictCardDef.Event b -> addEventSpec a b
        | ConflictCardDef.Attachment a, ConflictCardDef.Attachment b -> addAttachmentSpec a b
        | _ -> failwith "Cannot combine two different types of spec"

    let addStrongholdSpec spec1 spec2 = 
        strongholdSpec
            (if spec2.BonusStrength <> 0 then spec2.BonusStrength else spec1.BonusStrength)
            (if spec2.StartingHonor <> 0 then spec2.StartingHonor else spec1.StartingHonor)
            (if spec2.FatePerRound <> 0 then spec2.FatePerRound else spec1.FatePerRound)
            (if spec2.Influence <> 0 then spec2.Influence else spec1.Influence)

    let addProvinceSpec spec1 spec2 =
        provinceSpec
            (if spec2.Strength <> 0 then spec2.Strength else spec1.Strength)
            (if spec2.Element <> Element.Undefined then spec2.Element else spec1.Element)
                

    let addSpec spec1 spec2 = 
        match spec1, spec2 with
        | Dynasty a, Dynasty b -> addDynastySpec a b
        | Conflict a, Conflict b -> addConflictSpec a b
        | Stronghold a, Stronghold b ->addStrongholdSpec a b
        | Province a, Province b -> addProvinceSpec a b
        | Undefined, s -> s
        | s, Undefined -> s
        | _ -> failwith "Cannot combine two different types of spec"

    let addCards (card1:CardDef) (card2:CardDef) =
        card
            (if card2.Title <> Title "" then card2.Title else card1.Title)
            (addSpec card1.Spec card2.Spec)
            (if card2.Clan <> Clan.Undefined then card2.Clan else card1.Clan)
            (if card2.Set <> CardSet.Undefined then card2.Set else card1.Set)
            (card1.Actions @ card2.Actions)
            (card1.Triggers @ card2.Triggers)

    let inline (@+) (c1:CardDef) (c2:CardDef) = addCards c1 c2

    let title str = card (Title str) CardSpec.Undefined Clan.Undefined CardSet.Undefined [] []
    let clan cl = card (Title "") CardSpec.Undefined cl CardSet.Undefined [] []  

    let cardset cs = card (Title "") CardSpec.Undefined Clan.Undefined cs [] []  

    let action a = card (Title "") CardSpec.Undefined Clan.Undefined CardSet.Undefined [a] []   

    let trigger t = card (Title "") CardSpec.Undefined Clan.Undefined CardSet.Undefined [] [t]   

    let cardSpec spec = card (Title "") spec Clan.Undefined CardSet.Undefined [] []

    let dynastychar cost military political glory traits =
        cardSpec <| CardSpec.Dynasty (DynastyCardDef.Character (char cost military political glory traits))

    let conflictchar cost military political glory traits = 
        let spec = CardSpec.Conflict (ConflictCardDef.Character (char cost military political glory traits))
        card (Title "") spec Clan.Undefined CardSet.Undefined [] []

    let holding bonusStrength traits=
        cardSpec <| holdingSpec bonusStrength traits

    let event cost = cardSpec <| eventSpec cost
    let attachment cost bonusMilitary bonusPolitical traits = cardSpec <| attachmentSpec cost bonusMilitary bonusPolitical traits
    let province strength element = cardSpec <| provinceSpec strength element 
    let stronghold bonusStrength startingHonor fatePerRound influence = cardSpec <| strongholdSpec bonusStrength startingHonor fatePerRound influence

    let triggerDef name lifetime condition effect = 
      { Name = name
        Lifetime = lifetime
        Condition = condition
        Effect = effect }

    let addTriggerConditions c1 c2 =
        fun c cmd gs -> (c1 c cmd gs) && (c2 c cmd gs)

    let addEffect e1 e2 =
        fun card cmd -> 
            let tr = e1 card cmd
            let cont = e2 card cmd
            Transform.addContinuation tr (fun () -> cont)

    let addActionEffect e1 e2 =
        fun card -> 
            let tr = e1 card
            let cont = e2 card
            Transform.addContinuation tr (fun () -> cont)

    let addTriggers trg1 trg2 =
        triggerDef
            (if trg2.Name <> "" then trg2.Name else trg1.Name)
            (if trg2.Lifetime <> Lifetime.Undefined then trg2.Lifetime else trg1.Lifetime)
            (addTriggerConditions trg1.Condition trg2.Condition)
            (addEffect trg1.Effect trg2.Effect)


    let inline (@?+) (trg1:CardTriggerDef) (trg2:CardTriggerDef) = addTriggers trg1 trg2

    let actionDef name condition effect =
      { Name = name
        Condition = condition
        Effect = effect}

    let addActionConditions c1 c2 =
        fun c gs -> (c1 c gs) && (c2 c gs)

    let addActions  (a1:CardActionDef) (a2:CardActionDef) =
        actionDef
            (if a2.Name <> "" then a2.Name else a1.Name)
            (addActionConditions a1.Condition a2.Condition)
            (addActionEffect a1.Effect a2.Effect)

    let inline (@!+) (a1:CardActionDef) (a2:CardActionDef)= addActions a1 a2 
    

    // ----------------- Conditions -----------------------


    let always = fun _ _ _ -> true

    let tNothing = fun _ _ -> Transform.none ()
    let aNothing = fun _ -> Transform.none ()

    let tName name = triggerDef name Lifetime.Undefined always tNothing
    let tLifetime lifetime = triggerDef "" lifetime always tNothing
    let tCondition condition = triggerDef "" Lifetime.Undefined condition tNothing

    let tEffect effect = triggerDef "" Lifetime.Undefined always effect

    let anytime = fun _ _ -> true

    let aName name = actionDef name anytime aNothing
    let aCondition condition = actionDef "" condition aNothing
    let aEffect effect = actionDef "" anytime effect


