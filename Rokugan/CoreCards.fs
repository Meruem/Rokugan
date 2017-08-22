module CoreCards

open CardDef

let coreCards = [
    {
        Title = Title "Akodo Gunsō" 
        Spec = Dynasty (DynastyCardDef.Character {
            Cost = 2
            Clan = Clan.Lion
            MilitarySkill = Some 2
            PoliticalSkill = Some 1
            Glory = 2
            Traits = [Bushi]
            Set = CardSet.Core
            Ability = Ability "Pride and some stuff" })
    }

    {
        Title = Title "Adept of the Waves" 
        Spec = Dynasty (DynastyCardDef.Character {
        Cost = 2
        Clan = Clan.Phoenix
        MilitarySkill = Some 2
        PoliticalSkill = Some 2
        Glory = 2
        Traits = [Shugenja; Water]
        Set = CardSet.Core
        Ability = Ability "Gives covert"})
    }
        
    {
        Title = Title "Artisan Academy" 
        Spec = Dynasty (DynastyCardDef.Holding {
            Clan = Clan.Crane
            BonusStrength = 1
            Traits = [Academy]
            Set = CardSet.Core
            Ability = Ability "Play top conflict card"})
    }

    {
        Title = Title "Admit Defeat"
        Spec = Conflict (ConflictCardDef.Event {
            Clan = Clan.Crane
            Cost = 1
            Ability = Ability "Bow character or something" })
    }

    {
        Title = Title "Ancestral Daishō"
        Spec = Conflict (ConflictCardDef.Attachment {
            Clan = Clan.Dragon
            Cost = 1
            BonusMilitary = 2
            BonusPolitical = 0
            Traits = [Weapon]
            Ability = Ability "Ancestral restricted"})
    }

    {
        Title = Title "Defend the Wall"
        Spec = Province {
            Strength = 4
            Clan = Clan.Crab
            Element = Earth
            Ability = Ability "blabla" }
    }

    {
        Title = Title "Golden Plains Outpost"
        Spec = Stronghold {
            Clan = Clan.Unicorn
            BonusStrength = 0
            StartingHonor = 10
            FatePerRound = 7
            Influence = 10
            Ability = Ability "sda" }
    }
]