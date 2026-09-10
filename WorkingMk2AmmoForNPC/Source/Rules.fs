namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module internal Hashing =
    let tryJoaat (originalName: string) =
        if String.IsNullOrWhiteSpace(originalName) then
            None
        else
            let mutable hash = 0u
            let mutable valid = true
            for character in originalName do
                let lowered = Char.ToLowerInvariant(character)
                if int lowered > 0x7F then
                    valid <- false
                elif valid then
                    hash <- hash + uint32 (byte lowered)
                    hash <- hash + (hash <<< 10)
                    hash <- hash ^^^ (hash >>> 6)
            if not valid then
                None
            else
                hash <- hash + (hash <<< 3)
                hash <- hash ^^^ (hash >>> 11)
                hash <- hash + (hash <<< 15)
                if hash = 0u then None else Some hash

    let joaat originalName =
        tryJoaat originalName |> Option.defaultValue 0u

[<RequireQualifiedAccess>]
module internal OriginalComponentCatalog =
    let bindings : OriginalComponentBinding array =
        [|
            { ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_EXPLOSIVE"; Effect = Mk2AmmoEffect.Explosive }
            { ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_EXPLOSIVE"; Effect = Mk2AmmoEffect.Explosive }
            { ComponentName = "COMPONENT_PISTOL_MK2_CLIP_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { ComponentName = "COMPONENT_SMG_MK2_CLIP_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_PISTOL_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_SMG_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_PISTOL_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_SMG_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { ComponentName = "COMPONENT_PISTOL_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_SMG_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_TRACER"; Effect = Mk2AmmoEffect.Tracer }
        |]

[<RequireQualifiedAccess>]
module internal RuleCatalog =
    [<Literal>]
    let ExplosiveAmmoExplosionType = 45

    [<Literal>]
    let ExplosiveAmmoShotgunExplosionType = 61

    let private pumpShotgunExplosiveComponent =
        Hashing.joaat "COMPONENT_PUMPSHOTGUN_MK2_CLIP_EXPLOSIVE"

    let private tracerCadence componentName =
        match componentName with
        | "COMPONENT_PISTOL_MK2_CLIP_TRACER"
        | "COMPONENT_SNSPISTOL_MK2_CLIP_TRACER"
        | "COMPONENT_SMG_MK2_CLIP_TRACER"
        | "COMPONENT_ASSAULTRIFLE_MK2_CLIP_TRACER"
        | "COMPONENT_CARBINERIFLE_MK2_CLIP_TRACER"
        | "COMPONENT_SPECIALCARBINE_MK2_CLIP_TRACER"
        | "COMPONENT_BULLPUPRIFLE_MK2_CLIP_TRACER" ->
            TracerCadence.create 1 3
        | "COMPONENT_MARKSMANRIFLE_MK2_CLIP_TRACER" ->
            TracerCadence.create 0 3
        | "COMPONENT_COMBATMG_MK2_CLIP_TRACER" ->
            TracerCadence.create 1 5
        | "COMPONENT_REVOLVER_MK2_CLIP_TRACER" ->
            TracerCadence.create 1 0
        | _ ->
            TracerCadence.defaultValue

    let private createRule componentName componentHash effect =
        let explosionType =
            match effect with
            | Mk2AmmoEffect.Explosive when componentHash = pumpShotgunExplosiveComponent ->
                ExplosiveAmmoShotgunExplosionType
            | Mk2AmmoEffect.Explosive ->
                ExplosiveAmmoExplosionType
            | _ ->
                -1

        { ComponentName = componentName
          ComponentHash = componentHash
          Effect = effect
          TracerCadence = tracerCadence componentName
          ExplosionType = explosionType }

    let private rules =
        let values = ResizeArray<ComponentRule>()
        let seenComponents = HashSet<uint32>()

        for original in OriginalComponentCatalog.bindings do
            match Hashing.tryJoaat original.ComponentName with
            | Some componentHash when seenComponents.Add(componentHash) ->
                values.Add(
                    createRule
                        original.ComponentName
                        componentHash
                        original.Effect)
            | _ -> ()

        values.ToArray()

    let private tryWeaponHashFromComponentName (componentName: string) =
        let prefix = "COMPONENT_"
        let marker = "_CLIP_"
        if not (componentName.StartsWith(prefix, StringComparison.Ordinal)) then
            None
        else
            let startIndex = prefix.Length
            let markerIndex = componentName.IndexOf(marker, startIndex, StringComparison.Ordinal)
            if markerIndex <= startIndex then
                None
            else
                let weaponName =
                    "WEAPON_" + componentName.Substring(startIndex, markerIndex - startIndex)
                Hashing.tryJoaat weaponName

    let private rulesByWeapon =
        let grouped = Dictionary<uint32, ResizeArray<ComponentRule>>()
        for rule in rules do
            match tryWeaponHashFromComponentName rule.ComponentName with
            | Some weaponHash ->
                let bucket =
                    match grouped.TryGetValue(weaponHash) with
                    | true, value -> value
                    | _ ->
                        let value = ResizeArray<ComponentRule>()
                        grouped.Add(weaponHash, value)
                        value
                bucket.Add(rule)
            | None -> ()

        let indexed = Dictionary<uint32, ComponentRule array>()
        for pair in grouped do
            indexed.Add(pair.Key, pair.Value.ToArray())
        indexed

    let tryGetCandidates (weaponHash: uint32) =
        if weaponHash = 0u then
            None
        else
            match rulesByWeapon.TryGetValue(weaponHash) with
            | true, candidates -> Some candidates
            | _ -> None

    let tryResolveComponent
        (weaponHash: uint32)
        (hasComponent: uint32 -> bool) =
        match tryGetCandidates weaponHash with
        | None -> None
        | Some candidates ->
            let mutable resolved : ComponentRule option = None
            let mutable ambiguous = false
            let mutable index = 0
            while index < candidates.Length && not ambiguous do
                let candidate = candidates[index]
                if hasComponent candidate.ComponentHash then
                    match resolved with
                    | None -> resolved <- Some candidate
                    | Some _ -> ambiguous <- true
                index <- index + 1
            if ambiguous then None else resolved