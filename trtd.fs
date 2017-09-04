namespace GameTypes

type CardTitle = Title of string

type Clan = 
    | Lion
    | Crane
    | Crab
    | Phoenix
    | Scorpion
    | Unicorn
    | Dragon
    | Neutral

type Element = Fire | Water | Air | Earth | Void

type Trait =  Bushi | Shugenja | Water | Weapon | Academy
type CardSet = Core

type Ability = Ability of string 

type CharacterCardDef = {
    Cost : int
    Clan : Clan
    MilitarySkill : int option
    PoliticalSkill : int option
    Glory : int
    Traits : Trait list
    Set : CardSet
    Ability : Ability }

type HoldingCardDef = {
    Clan : Clan
    BonusStrength : int     
    Traits : Trait list   
    Ability : Ability
    Set : CardSet }

type DynastyCardDef = 
    | Character of CharacterCardDef
    | Holding of HoldingCardDef

type EventCardDef = {
    Clan : Clan
    Cost : int
    Ability : Ability }

type AttachmentCardDef = {
    Cost : int
    Clan : Clan
    BonusMilitary : int 
    BonusPolitical : int 
    Traits : Trait list
    Ability : Ability }

type StrongholdCardDef = {
    Clan : Clan
    BonusStrength : int
    StartingHonor : int
    FatePerRound : int
    Influence : int
    Ability : Ability }


type ConflictCardDef = 
    | Character of CharacterCardDef
    | Event of EventCardDef
    | Attachment of AttachmentCardDef

type ProvinceCardDef = {
    Strength : int
    Clan : Clan
    Element : Element
    Ability : Ability }

type RoleCardDef = {
    Ability : Ability
    Traits : Trait list }

type CardSpec = 
    | Dynasty of DynastyCardDef
    | Conflict of ConflictCardDef
    | Stronghold of StrongholdCardDef
    | Province of ProvinceCardDef
    | Role of RoleCardDef

type CardDef = {
    Title : CardTitle
    Spec : CardSpec }

type Player = Player1 | Player2

type CardState = Bowed | Honored | Dishonored | Hidden |Revealed | Broken
type CardId = CardId of int

type ZoneName = Hand | DynastyDiscard | ConflictDiscard | DynastyInProvinces | Home | Conflict | Province | Stronghold | StrongholdProvince | DynastyDeck | ConflictDeck

type Card = {
    Id : CardId
    Title : CardTitle
    Owner : Player
    States : CardState list
    Fate : int 
    Zone : ZoneName }


type Deck = 
    Deck of Card list
        member this.Cards = 
            let (Deck lst) = this
            lst

type PlayerFlagEnum = Passed

//defines when the flag or trigger is cleared
type Lifetime = Round | Phase | Game | Once
type ConflictType = Military | Political

type PlayerFlag =
  { Lifetime : Lifetime
    Flag : PlayerFlagEnum}

type PlayerState = {
    Bid : int option
    ConflictDeck : Deck
    DynastyDeck : Deck
    Honor : int
    Fate : int
    Flags : PlayerFlag list
    CardsInPlay : Map<CardId, Card>
    DeclaredConflicts : ConflictType option list }
    with
        member this.Home = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = Home)
        member this.DynastyDiscard = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = DynastyDiscard)
        member this.ConflictDiscard = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = ConflictDiscard)
        member this.Hand = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = Hand)
        member this.DynastyInProvinces = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = DynastyInProvinces)
        member this.Conflict = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = Conflict)
        member this.Stronghold = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = Stronghold then Some c else None)
        member this.StrongholdProvince = this.CardsInPlay |> Map.pick (fun _ c -> if c.Zone = StrongholdProvince then Some c else None)
        member this.Provinces = this.CardsInPlay |> Map.filter (fun _ c -> c.Zone = Province)
        member this.CardsInPlayList : Card list = this.CardsInPlay |> Map.toList |> List.map (fun (_, c) -> c)


type GameEnd = Player1Won | Player2Won 
type GamePhase = Dynasty | Draw | Conflict | Fate | Regroup | End of GameEnd 
type RingState = Unclaimed | Contested | Claimed of Player
type YesNo = Yes | No

type Ring = 
  { Element : Element
    State : RingState
    Fate : int }

type GameState = 
  { TurnNumber : int
    GamePhase : GamePhase
    ActivePlayer : Player
    FirstPlayer : Player
    Triggers : GameTrigger list
    Player1State : PlayerState
    Player2State : PlayerState 
    Rings : Ring list
    Actions : PlayerAction list }
and GameTrigger = 
  { Name : string
    Lifetime : Lifetime
    Condition : GameState -> bool
    Action : GameState -> GameState}
and PlayerActionType = 
    | Pass
    | PlayCharacter of CardTitle
    | ActivateAction
    | Choice of int * string
    | YesNoChoice of YesNo * string
    | DeclareAttack of ConflictType * Element
    | ChooseAttacker of Card 
    | ChooseDefender of Card
    | ChooseProvince of Card
    | ChooseCharacter of Card * string
and PlayerAction = 
  { Type : PlayerActionType
    Action : GameState -> GameState }

type InitialPlayerConfig = {
    Player : Player
    ConflictDeck : CardTitle list
    DynastyDeck : CardTitle list
    Stonghold : CardTitle
    StrongholdProvince : CardTitle
    Provinces : CardTitle list }    


module Utils

open GameTypes

let chooseRandomPlayer () = 
    let rnd = System.Random()
    if rnd.Next(2) = 0 then Player1 else Player2

let replaceListElementi newele pos lst =
    let (_, lst') = 
        lst 
        |> List.fold (fun (i, acc) d -> 
            let c = if i = pos then newele else d
            (i+1, List.append acc [c])) (0, [])
    lst'

let replaceListElement newele pred lst =
    lst 
    |> List.fold (fun acc d -> 
        let c = if pred d then newele else d
        List.append acc [c]) []

let listSplitBy pred lst =
    List.foldBack (fun item (x,y) -> if pred item then (item::x,y) else (x, item::y)) lst ([],[])

let mutable lastId = 1
let newId () = 
    lastId <- lastId + 1
    lastId

module CardDef

open GameTypes

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

let character cardDef = 
    match cardDef.Spec with 
    | CardSpec.Dynasty d ->
        match d with 
        | DynastyCardDef.Character c -> Some c
        | _ -> None
    | CardSpec.Conflict c ->
        match c with
        | ConflictCardDef.Character char -> Some char
        | _ -> None
    | _ -> None


module CardRepository
open CoreCards
open GameTypes
open System
let allCards = coreCards
let allCardsMap = [for card in allCards do yield card.Title, card] |> Map.ofList
let fail title = failwithf "Unknown card %s" (string title)

let getCard (title : CardTitle) = 
    if allCardsMap.ContainsKey title then allCardsMap.[title] else failwith ("Unknown card " + string title)

let getDynastyCard (title:CardTitle) =
    let card = getCard title
    match card.Spec with 
    | CardSpec.Dynasty d -> d
    | _ -> fail title

let getCharacterCard (title:CardTitle) =
    let card = getCard title
    match card.Spec with 
    | CardSpec.Dynasty d -> 
        match d with 
        | DynastyCardDef.Character c -> c
        | _ -> fail title
    | _ -> fail title

let getStrongholdCard title =
    let card = getCard title  
    match card.Spec with | CardSpec.Stronghold s -> s | _ -> fail title


module Card

open GameTypes

let createCard title player zone =
  { Id = CardId (Utils.newId ())
    Title = title
    Owner = player
    States = []
    Fate = 0 
    Zone = zone}

let createProviceCard title player = createCard title player Province

let createStrongholdCard title player = createCard title player Stronghold

let removeCardState state (card:Card) =
     { card with States = card.States |> List.filter (fun s -> s <> state) }

let addCardState state card = {card with States = state :: card.States }

let hasState state card = card.States |> List.contains state

let isBowed = hasState Bowed

let character card = 
    let cardDef = CardRepository.getCard card.Title
    CardDef.character cardDef 

let isCharWithValue cType card = 
    let cardDef = CardRepository.getCard card.Title
    match CardDef.character cardDef with
    | None -> false 
    | Some char -> 
        match cType with
        | Military -> Option.isSome char.MilitarySkill
        | Political -> Option.isSome char.PoliticalSkill

let revealProvince province = { province with States = Revealed :: province.States}

let charSkillValue cType card =
    let cardDef = CardRepository.getCard card.Title
    match CardDef.character cardDef with
    | None -> None 
    | Some char -> 
        match cType with
        | Military -> char.MilitarySkill
        | Political ->char.PoliticalSkill

let honorCard card = 
    if hasState Dishonored card then removeCardState Dishonored card
    else if not (hasState Honored card) then addCardState Honored card
        else card

let dishonorCard card = 
    if hasState Honored card then removeCardState Honored card
    else if not (hasState Dishonored card) then addCardState Dishonored card
        else card

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

module PlayerState

open GameTypes
open System.Security.Policy

let otherPlayer player = if player = Player1 then Player2 else Player1
let hasPlayerFlag flag playerState =
    playerState.Flags |> List.exists (fun ps -> ps.Flag = flag)

let changeBid bid playerState = { playerState with Bid = Some bid }

let hasPassed = hasPlayerFlag PlayerFlagEnum.Passed

let pass ps = {ps with Flags = { Lifetime = Phase; Flag = Passed } :: ps.Flags}

let addHonor honor (playerState:PlayerState) = 
    { playerState with Honor = playerState.Honor + honor}

let recycleDynastyDiscard ps =
    let (discard, other) = ps.CardsInPlay |> Map.toList |> Utils.listSplitBy (fun c -> c.Zone = DynastyDiscard)
    { ps with 
        CardsInPlay = other
        DynastyDeck = discard |> List.map (fun c -> {c with Zone = DynastyDeck}) |> Deck |> Deck.shuffleDeck }

let recycleConflictDiscard ps =
    let (discard, other) = ps.CardsInPlay |> Utils.listSplitBy (fun  c -> c.Zone = ConflictDiscard)
    { ps with 
        CardsInPlay = other
        ConflictDeck = discard |> List.map (fun c -> {c with Zone = ConflictDeck}) |> Deck |> Deck.shuffleDeck }

let rec drawCardFromDynastyDeck (playerState : PlayerState) = 
    match Deck.drawCardFromDeck playerState.DynastyDeck with
    | Some (card, rest) -> card, { playerState with DynastyDeck = rest }
    | None -> playerState |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromDynastyDeck      

let rec drawCardFromConflictDeck (ps : PlayerState) = 
    match Deck.drawCardFromDeck ps.ConflictDeck with
    | Some (card, rest) -> { ps with ConflictDeck = rest; CardsInPlay = ({card with Zone = Hand} :: ps.CardsInPlay) }
    | None -> ps |> addHonor -5 |> recycleDynastyDiscard |> drawCardFromConflictDeck      

let drawConflictCards n playerState = [1..n] |> List.fold (fun pstate i -> pstate |> drawCardFromConflictDeck) playerState

let initializePlayerState (initialConfig:InitialPlayerConfig) =
    let init zone title = Card.createCard title initialConfig.Player zone
    let conflictDeck = initialConfig.ConflictDeck |> List.map (init ConflictDeck) |> Deck
    let hand, conflictDeck' = Deck.getCardsFromDeck 4 conflictDeck
    let dynastyDeck = initialConfig.DynastyDeck |> List.map (init DynastyDeck) |> Deck
    let dynastyHand, dynastyDeck' = Deck.getCardsFromDeck 4 dynastyDeck
    let initProvince title = Card.createProviceCard title initialConfig.Player
    let strongholdCard = Card.createStrongholdCard initialConfig.Stonghold initialConfig.Player
    let stronghold = CardRepository.getStrongholdCard initialConfig.Stonghold
    {
        Bid = None
        Honor = stronghold.StartingHonor
        Fate = 0
        ConflictDeck = conflictDeck'
        DynastyDeck = dynastyDeck'
        CardsInPlay = 
          [ hand |> List.map (fun c -> {c with Zone = Hand })  
            [strongholdCard] 
            dynastyHand |> List.map (fun c -> { c with Zone = DynastyInProvinces})
            initialConfig.Provinces |> List.map  initProvince
            [initProvince initialConfig.StrongholdProvince]]
            |> List.concat
        Flags = []
        DeclaredConflicts = []
    }   

let addFateToPlayer fate (playerState:PlayerState) = 
    { playerState with Fate = playerState.Fate + fate }      

let getPlayableDynastyPositions (ps:PlayerState) =
    ps.DynastyInProvinces 
    |> List.mapi (fun i card -> 
        let cardDef = CardRepository.getDynastyCard card.Title
        let remainingFate = 
            match cardDef with 
            | Holding _ -> -1 
            | DynastyCardDef.Character c -> ps.Fate - c.Cost
        let hidden = List.contains Hidden card.States
        if remainingFate >= 0 && (not hidden) then (i, remainingFate) else (-1,0))
    |> List.filter (fun (i, _) -> i <> -1)
 
let cleanPhaseFlags playerState = 
    let flags = playerState.Flags |> List.filter (fun f -> f.Lifetime <> Phase) 
    { playerState with Flags = flags }   

let changeProvinceState province stateChange (ps:PlayerState) = 
    if ps.StrongholdProvince.Id = province.Id then 
        {ps with StrongholdProvince = stateChange ps.StrongholdProvince }
    else 
        let provinces = 
            [for p in ps.Provinces do 
                if p.ProvinceCard.Id = province.ProvinceCard.Id then 
                    yield stateChange p 
                else yield p]
        {ps with Provinces = provinces}

let revealProvince province = changeProvinceState province Card.revealProvince

module GameState
open GameTypes
open System
open PlayerState

let changePlayerState player playerStateChange gs =
    match player with
    | Player1 -> {gs with Player1State = gs.Player1State |> playerStateChange}
    | Player2 -> {gs with Player2State = gs.Player2State |> playerStateChange}

let changeActivePlayerState playerStateChange gs = changePlayerState gs.ActivePlayer playerStateChange gs
let changeOtherPlayerState playerStateChange gs = changePlayerState (otherPlayer gs.ActivePlayer) playerStateChange gs

let addSecondPlayer1Fate gs =
    let otherPl = otherPlayer gs.FirstPlayer
    let add1Fate (ps:PlayerState) = {ps with Fate = ps.Fate + 1}
    gs |> changePlayerState otherPl add1Fate

let hasPlayerPassed player gs =
    match player with
    | Player1 -> hasPassed gs.Player1State
    | Player2 -> hasPassed gs.Player2State

let switchActivePlayer gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPlayerPassed otherPl gs then gs
    else { gs with ActivePlayer = otherPl}

let playerState player gs =
    match player with
    | Player1 -> gs.Player1State
    | Player2 -> gs.Player2State

let activePlayerState gs = playerState gs.ActivePlayer gs

let otherPlayerState gs = playerState (otherPlayer gs.ActivePlayer) gs

let passActive = changeActivePlayerState pass

let addChoiceActions actionList gs =
    {gs with Actions = actionList}

let cleanPhaseFlags gs =
    { gs with 
        Player1State = cleanPhaseFlags gs.Player1State
        Player2State = cleanPhaseFlags gs.Player2State }

let removeFateFromRing ring gs =
    { gs with Rings = gs.Rings |> Utils.replaceListElement { ring with Fate = 0 } (fun r -> r.Element = ring.Element) }

let cardsInPlay gs = 
    List.append (PlayerState.cardsInPlay gs.Player1State) (PlayerState.cardsInPlay gs.Player2State)

module Actions

open GameTypes
open GameState

let createChoiceActions nextAction desc min max =
    [min..max]
        |> List.map (fun i -> 
            { Action = nextAction i 
              Type = Choice (i, desc) })

let createYesNoActions nextAction desc =
    [ {Action = nextAction Yes; Type = YesNoChoice (Yes, desc)}
      {Action = nextAction No; Type = YesNoChoice (No, desc)}]

let createChooseCharacterInPlayActions nextAction desc gs =
    gs 
    |> cardsInPlay 
    |> List.filter (fun c -> Option.isSome (Card.character c))
    |> List.map (fun c -> 
        {Action = nextAction c; Type = ChooseCharacter (c, desc) })
            


module Dynasty

open GameTypes
open PlayerState
open GameState

let revealAllDynastyCardsAtProvinces gs =
    let removeHiddenState = Card.removeCardState Hidden
    let removeHiddenFromPlayerState pState =
        { pState with DynastyInProvinces = pState.DynastyInProvinces.Cards |> List.map removeHiddenState |> Zone }
    { gs with 
        Player1State = removeHiddenFromPlayerState gs.Player1State
        Player2State = removeHiddenFromPlayerState gs.Player2State }

let playDynastyCard position addFate gs =
    let changeState state =
        let card = state.DynastyInProvinces.Cards.[position]
        let card' = { card with Fate = addFate}
        let home = card' :: state.Home.Cards
        let newCard, state' = PlayerState.drawCardFromDynastyDeck state
        let cardDef = CardRepository.getCharacterCard card.Title
        // add new dynasty card
        let dynastyInProvinces' = 
            state.DynastyInProvinces.Cards 
            |> Utils.replaceListElementi newCard position
        {state' with 
            Home = Zone home
            DynastyInProvinces = Zone dynastyInProvinces'
            Fate = state'.Fate - cardDef.Cost - addFate}
    gs |> GameState.changeActivePlayerState changeState    

let collectFateFromStronghold gs =
    let stronghold (pState:PlayerState) = pState.Stonghold.StrongholdCard.Title |> CardRepository.getCard
    let fate pState =
        match (stronghold pState).Spec with 
        | Stronghold s -> s.FatePerRound
        | _ -> failwith "Stronghold card is not stronghold card"

    let p1Add = fate gs.Player1State
    let p2Add = fate gs.Player2State
    { gs with 
        Player1State = (PlayerState.addFateToPlayer p1Add gs.Player1State)    
        Player2State = (PlayerState.addFateToPlayer p2Add gs.Player2State)  }        

let add1fateIfPassedFirst gs =
    let otherPl = otherPlayer gs.ActivePlayer
    if hasPassed (playerState otherPl gs) then gs
    else 
        let add1Fate (state:PlayerState) = {state with Fate = state.Fate + 1} 
        gs |> changeActivePlayerState add1Fate

let rec addDynastyPhaseActions gs =
    let ps = GameState.activePlayerState gs
    let actions = 
        getPlayableDynastyPositions ps
        |> List.map (fun (pos, remainingFate) ->
            let nextAction fate = playDynastyCard pos fate >> GameState.switchActivePlayer >> addDynastyPhaseActions
            { 
                Action = Actions.createChoiceActions nextAction "Add fate" 0 remainingFate  |> addChoiceActions
                Type = PlayCharacter ps.DynastyInProvinces.Cards.[pos].Title })
    if PlayerState.hasPassed ps then { gs with Actions = actions}
    else 
        let passAction = 
            { Type = PlayerActionType.Pass 
              Action = passActive >> add1fateIfPassedFirst >> switchActivePlayer >> addDynastyPhaseActions } 
        let actions' = passAction :: actions
        { gs with Actions = actions'}     

module Draw

open GameTypes
open PlayerState
open GameState

let applyBids pl1Bid pl2Bid gotoNextPhase gs =
    { gs with
        Actions = []
        Player1State = gs.Player1State |> changeBid pl1Bid |> addHonor (pl2Bid - pl1Bid) |> drawConflictCards pl1Bid
        Player2State = gs.Player2State |> changeBid pl2Bid |> addHonor (pl1Bid - pl2Bid) |> drawConflictCards pl2Bid}
    |> gotoNextPhase

let getDrawPhaseActions gotoNextPhase gs =
    let nextActionPl1 pl1Bid gs = { gs with Actions = createChoiceActions (fun i -> applyBids pl1Bid i gotoNextPhase) "Player 2 bid" 1 5 } 
    let nextActionPl2 pl2Bid gs = { gs with Actions = createChoiceActions (fun i -> applyBids i pl2Bid gotoNextPhase) "Player 1 bid" 1 5 } 
    { gs with 
        Actions = List.append (createChoiceActions nextActionPl1 "Player 1 bid" 1 5) (createChoiceActions nextActionPl2 "Player 2 bid" 1 5) }        

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

let resolveRingEffect ring yesNo (gs:GameState) = 
    match yesNo with 
    | No -> gs
    | Yes -> 
        match ring.Element with
        | Fire -> 
            let honorWithFire char gs = gs
            { gs with Actions = Actions.createChooseCharacterInPlayActions honorWithFire "Choose character to honor" gs}
        | _ -> gs

let askToResolveRingEffect ring gs = 
    { gs with Actions = Actions.createYesNoActions (resolveRingEffect ring) "Resolve ring effect" }
    

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
    { gs with Actions = passAction :: actions }

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
    { gs with Actions = passAction :: actions }

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
  
module Game

open GameTypes

open PlayerState
open GameState
open Conflict

let gotoDrawPhase gotoNextPhase gs =
    { gs with 
        GamePhase = Draw
        ActivePlayer = gs.FirstPlayer} |> Draw.getDrawPhaseActions gotoNextPhase 

let gotoConflictPhase gs = 
    { gs with
        GamePhase = Conflict
        ActivePlayer = gs.FirstPlayer }
    |> getConflictPhaseActions

let gotoDynastyPhase gotoNextPhase gs = 
    gs
    |> Dynasty.revealAllDynastyCardsAtProvinces
    |> Dynasty.collectFateFromStronghold
    |> Triggers.addDynastyPassTrigger gotoNextPhase
    |> Dynasty.addDynastyPhaseActions   

let rec gotoNextPhase gs =
    let gs' = gs |> cleanPhaseFlags |> Triggers.cleanPhaseTriggers
    match gs.GamePhase with
    | Dynasty -> gs' |> gotoDrawPhase gotoNextPhase
    | Draw -> gs' |> gotoConflictPhase 
    | _ -> gs'

let startGame = gotoDynastyPhase gotoNextPhase

let createStartingRings = 
  [ {Element = Fire; State = Unclaimed; Fate = 0}
    {Element = Element.Water; State = Unclaimed; Fate = 0}
    {Element = Air; State = Unclaimed; Fate = 0}
    {Element = Earth; State = Unclaimed; Fate = 0}
    {Element = Void; State = Unclaimed; Fate = 0}] 

let initializeGameState playerConfig1 playerConfig2 = 
    let firstPlayer = Utils.chooseRandomPlayer ()
    { 
        Rings = createStartingRings
        Triggers = []
        TurnNumber = 1
        ActivePlayer = firstPlayer
        Actions = []
        GamePhase = Dynasty
        FirstPlayer = firstPlayer
        Player1State = initializePlayerState playerConfig1
        Player2State = initializePlayerState playerConfig2  }
    |> addSecondPlayer1Fate
    |> Triggers.addWinConditionsTriggers

let playAction n gs =
    if n > gs.Actions.Length then gs
    else
        gs |> gs.Actions.[n].Action |> Triggers.applyTriggers    
