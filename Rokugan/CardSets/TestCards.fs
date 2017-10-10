module TestCards

open RokuganShared
open GameTypes

let ``Generic 1/1`` = 
    {   Title = Title "Generic 1/1" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Character {
            Cost = 1
            Clan = Clan.Lion
            MilitarySkill = Some 1
            PoliticalSkill = Some 1
            Glory = 0
            Traits = []
            Set = CardSet.Core })
        Ability = []    
    }

let ``Generic 2/2`` =
     {  Title = Title "Generic 2/2" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Character {
            Cost = 2
            Clan = Clan.Lion
            MilitarySkill = Some 2
            PoliticalSkill = Some 2
            Glory = 0
            Traits = []
            Set = CardSet.Core })
        Ability = []    
    }
        
let ``Holding`` =
    {
        Title = Title "Holding" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Holding {
            Clan = Clan.Crane
            BonusStrength = 1
            Traits = []
            Set = CardSet.Core })
        Ability = []    
    }

let ``Event`` =
    {
        Title = Title "Event"
        Spec = CardSpec.Conflict (ConflictCardDef.Event {
            Clan = Clan.Crane
            Cost = 1 })
        Ability = []    
    }

let ``Attachment`` =
    {
        Title = Title "Attachment"
        Spec = CardSpec.Conflict (ConflictCardDef.Attachment {
            Clan = Clan.Dragon
            Cost = 1
            BonusMilitary = 2
            BonusPolitical = 0
            Traits = [Weapon] })
        Ability = []    
    }

let ``Province str 4``=
    {
        Title = Title "Province str 4"
        Spec = CardSpec.Province {
            Strength = 4
            Clan = Clan.Crab
            Element = Earth }
        Ability =[] 
    }

let ``Stronghold`` =
    {
        Title = Title "Stronghold"
        Spec = CardSpec.Stronghold {
            Clan = Clan.Unicorn
            BonusStrength = 0
            StartingHonor = 10
            FatePerRound = 7
            Influence = 10 }
        Ability = []    
    }

let testCards = 
  [ ``Generic 1/1``
    ``Generic 2/2``
    ``Holding``
    ``Event``
    ``Attachment``
    ``Province str 4``
    ``Stronghold``]
