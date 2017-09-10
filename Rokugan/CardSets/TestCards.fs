module TestCards

open GameTypes

let testCards = [
    {
        Title = Title "Generic 1/1" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Character {
            Cost = 1
            Clan = Clan.Lion
            MilitarySkill = Some 1
            PoliticalSkill = Some 1
            Glory = 0
            Traits = []
            Set = CardSet.Core
            Ability = Ability "none" })
    }

    {
        Title = Title "Generic 2/2" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Character {
            Cost = 2
            Clan = Clan.Lion
            MilitarySkill = Some 2
            PoliticalSkill = Some 2
            Glory = 0
            Traits = []
            Set = CardSet.Core
            Ability = Ability "none" })
    }
        
    {
        Title = Title "Holding" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Holding {
            Clan = Clan.Crane
            BonusStrength = 1
            Traits = []
            Set = CardSet.Core
            Ability = Ability "none"})
    }

    {
        Title = Title "Event"
        Spec = CardSpec.Conflict (ConflictCardDef.Event {
            Clan = Clan.Crane
            Cost = 1
            Ability = Ability "none" })
    }

    {
        Title = Title "Attachment"
        Spec = CardSpec.Conflict (ConflictCardDef.Attachment {
            Clan = Clan.Dragon
            Cost = 1
            BonusMilitary = 2
            BonusPolitical = 0
            Traits = [Weapon]
            Ability = Ability "none"})
    }

    {
        Title = Title "Province str 4"
        Spec = CardSpec.Province {
            Strength = 4
            Clan = Clan.Crab
            Element = Earth
            Ability = Ability "none" }
    }

    {
        Title = Title "Stronghold"
        Spec = CardSpec.Stronghold {
            Clan = Clan.Unicorn
            BonusStrength = 0
            StartingHonor = 10
            FatePerRound = 7
            Influence = 10
            Ability = Ability "none" }
    }
]