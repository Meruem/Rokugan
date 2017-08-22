module CardDef

open System

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

type CharacterCardDef = 
    {
        Cost : int
        Clan : Clan
        MilitarySkill : int option
        PoliticalSkill : int option
        Glory : int
        Traits : Trait list
        Set : CardSet
        Ability : Ability
    }

type HoldingCardDef = 
    {
        Clan : Clan
        BonusStrength : int     
        Traits : Trait list   
        Ability : Ability
        Set : CardSet
    }

type DynastyCardDef = 
    | Character of CharacterCardDef
    | Holding of HoldingCardDef

type EventCardDef =
    {
        Clan : Clan
        Cost : int
        Ability : Ability
    }

type AttachmentCardDef = 
    {
        Cost : int
        Clan : Clan
        BonusMilitary : int 
        BonusPolitical : int 
        Traits : Trait list
        Ability : Ability
     }

type StrongholdCardDef = 
    {
        Clan : Clan
        BonusStrength : int
        StartingHonor : int
        FatePerRound : int
        Influence : int
        Ability : Ability
     }


type ConflictCardDef = 
    | Character of CharacterCardDef
    | Event of EventCardDef
    | Attachment of AttachmentCardDef

type ProvinceCardDef = 
    {
        Strength : int
        Clan : Clan
        Element : Element
        Ability : Ability
    }

type RoleCardDef = 
    {
        Ability : Ability
        Traits : Trait list
    }

type CardSpec = 
    | Dynasty of DynastyCardDef
    | Conflict of ConflictCardDef
    | Stronghold of StrongholdCardDef
    | Province of ProvinceCardDef
    | Role of RoleCardDef

type CardDef = 
    {
        Title : CardTitle
        Spec : CardSpec
    }

let dynastyCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | Dynasty d -> yield d | _ -> ()]

let filterConflictCards = List.filter (fun card -> match card.Spec with | Conflict c -> true | _ -> false)
let filterDynastyCards = List.filter (fun card -> match card.Spec with | Dynasty c -> true | _ -> false)
let filterStrongholdCards = List.filter (fun card -> match card.Spec with | Stronghold c -> true | _ -> false)
let filterProvinceCards = List.filter (fun card -> match card.Spec with | Province c -> true | _ -> false)

let conflictCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | Conflict c -> yield c | _ -> ()]
let strongholdCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | Stronghold c -> yield c | _ -> ()]    
let provinceCards (cardList : CardDef list) =
    [for card in cardList do match card.Spec with | Province c -> yield c | _ -> ()]   
    