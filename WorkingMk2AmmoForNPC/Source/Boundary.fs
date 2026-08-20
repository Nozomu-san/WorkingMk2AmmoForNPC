namespace WorkingMk2AmmoForNPC.Source

[<RequireQualifiedAccess>]
module internal Boundary =
    [<Literal>]
    let ArchiveName =
        "WorkingMk2AmmoForNPC Archive v4 revision 8 - reaction-aware ragdoll"

    [<Literal>]
    let LanguageVersion = "F# 10"

    [<Literal>]
    let TargetFramework = "net10.0"

    [<Literal>]
    let RuntimeModel =
        "Autonomous Script4 executable with one responsibility: reproduce GTA Online-style special-ammo damage for NPC shooters. Shooter discovery remains bounded. A uniquely installed special-ammo clip component is authoritative for NPC rule identity, while active-component and current-ammo checks only disambiguate ambiguous component state. Frame-transient shooting and impact observations are sampled once per observed host frame before the slower damage-settlement pass, so one-frame impact coordinates are not hidden behind the 33 ms processing interval. Hit resolution is evidence-first: captured nearby candidates plus actual damage evidence and impact geometry are authoritative, while AI combat-target lookup is not on any special-ammo critical path. Explosive settlement requests ragdoll through SGO; pure invincibility remains protected while reaction-preserving invincibility can still perform the intended cover-breaking physical reaction. No full-world discovery exists in the runtime path."

    [<Literal>]
    let DeferredScope =
        "User-owned JSON configuration and JSON/addon loading remain intentionally deferred."

    [<Literal>]
    let SourceFileCount = 8

    [<Literal>]
    let ImplementedEffectCount = 6

    let EmbeddedSpecialAmmoRuleCount = RuleCatalog.ruleCount