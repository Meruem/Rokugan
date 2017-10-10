module CoreCards

open RokuganShared
open GameTypes

let coreCards = [
    {
        Title = Title "Akodo Gunsō" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Character {
            Cost = 2
            Clan = Clan.Lion
            MilitarySkill = Some 2
            PoliticalSkill = Some 1
            Glory = 2
            Traits = [Bushi]
            Set = CardSet.Core})
        Ability = []     //Ability "Pride and some stuff" })
    }

    {
        Title = Title "Adept of the Waves" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Character {
            Cost = 2
            Clan = Clan.Phoenix
            MilitarySkill = Some 2
            PoliticalSkill = Some 2
            Glory = 2
            Traits = [Shugenja; Water]
            Set = CardSet.Core}) //Ability "Gives covert"})
        Ability = []
    }
        
    {
        Title = Title "Artisan Academy" 
        Spec = CardSpec.Dynasty (DynastyCardDef.Holding {
            Clan = Clan.Crane
            BonusStrength = 1
            Traits = [Academy]
            Set = CardSet.Core}) // Ability "Play top conflict card"})
        Ability = []            
    }

    {
        Title = Title "Admit Defeat"
        Spec = CardSpec.Conflict (ConflictCardDef.Event {
            Clan = Clan.Crane
            Cost = 1 }) //Ability "Bow character or something" })
        Ability = []            
    }

    {
        Title = Title "Ancestral Daishō"
        Spec = CardSpec.Conflict (ConflictCardDef.Attachment {
            Clan = Clan.Dragon
            Cost = 1
            BonusMilitary = 2
            BonusPolitical = 0
            Traits = [Weapon]}) //Ability "Ancestral restricted"})
        Ability = []    
    }

    {
        Title = Title "Defend the Wall"
        Spec = CardSpec.Province {
            Strength = 4
            Clan = Clan.Crab
            Element = Earth}
        Ability = []    
    }

    {
        Title = Title "Golden Plains Outpost"
        Spec = CardSpec.Stronghold {
            Clan = Clan.Unicorn
            BonusStrength = 0
            StartingHonor = 10
            FatePerRound = 7
            Influence = 10 }
        Ability = []    
    }
]
