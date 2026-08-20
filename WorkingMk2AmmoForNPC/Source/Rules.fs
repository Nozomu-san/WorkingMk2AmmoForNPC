namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic
open StandardGameOperations.Source

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
            if not valid then None
            else
                hash <- hash + (hash <<< 3)
                hash <- hash ^^^ (hash >>> 11)
                hash <- hash + (hash <<< 15)
                if hash = 0u then None else Some hash

    let joaat originalName =
        tryJoaat originalName |> Option.defaultValue 0u

[<RequireQualifiedAccess>]
module internal OriginalNameCatalog =
    let rules : OriginalNameRule array =
        [|
            { WeaponName = "WEAPON_HEAVYSNIPER_MK2"; ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_EXPLOSIVE"; AmmoTypeName = "AMMO_SNIPER_EXPLOSIVE"; Effect = Mk2AmmoEffect.Explosive }
            { WeaponName = "WEAPON_PUMPSHOTGUN_MK2"; ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_EXPLOSIVE"; AmmoTypeName = "AMMO_SHOTGUN_EXPLOSIVE"; Effect = Mk2AmmoEffect.Explosive }

            { WeaponName = "WEAPON_PISTOL_MK2"; ComponentName = "COMPONENT_PISTOL_MK2_CLIP_HOLLOWPOINT"; AmmoTypeName = "AMMO_PISTOL_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { WeaponName = "WEAPON_SNSPISTOL_MK2"; ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_HOLLOWPOINT"; AmmoTypeName = "AMMO_PISTOL_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { WeaponName = "WEAPON_REVOLVER_MK2"; ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_HOLLOWPOINT"; AmmoTypeName = "AMMO_PISTOL_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { WeaponName = "WEAPON_PUMPSHOTGUN_MK2"; ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_HOLLOWPOINT"; AmmoTypeName = "AMMO_SHOTGUN_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }
            { WeaponName = "WEAPON_SMG_MK2"; ComponentName = "COMPONENT_SMG_MK2_CLIP_HOLLOWPOINT"; AmmoTypeName = "AMMO_SMG_HOLLOWPOINT"; Effect = Mk2AmmoEffect.HollowPoint }

            { WeaponName = "WEAPON_ASSAULTRIFLE_MK2"; ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_RIFLE_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { WeaponName = "WEAPON_CARBINERIFLE_MK2"; ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_RIFLE_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { WeaponName = "WEAPON_SPECIALCARBINE_MK2"; ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_RIFLE_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { WeaponName = "WEAPON_BULLPUPRIFLE_MK2"; ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_RIFLE_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { WeaponName = "WEAPON_HEAVYSNIPER_MK2"; ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_SNIPER_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { WeaponName = "WEAPON_MARKSMANRIFLE_MK2"; ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_SNIPER_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { WeaponName = "WEAPON_PUMPSHOTGUN_MK2"; ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_SHOTGUN_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }
            { WeaponName = "WEAPON_COMBATMG_MK2"; ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_ARMORPIERCING"; AmmoTypeName = "AMMO_MG_ARMORPIERCING"; Effect = Mk2AmmoEffect.ArmorPiercing }

            { WeaponName = "WEAPON_ASSAULTRIFLE_MK2"; ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_RIFLE_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_CARBINERIFLE_MK2"; ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_RIFLE_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_SPECIALCARBINE_MK2"; ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_RIFLE_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_BULLPUPRIFLE_MK2"; ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_RIFLE_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_COMBATMG_MK2"; ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_MG_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_HEAVYSNIPER_MK2"; ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_SNIPER_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_MARKSMANRIFLE_MK2"; ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_SNIPER_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_PISTOL_MK2"; ComponentName = "COMPONENT_PISTOL_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_PISTOL_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_PUMPSHOTGUN_MK2"; ComponentName = "COMPONENT_PUMPSHOTGUN_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_SHOTGUN_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_REVOLVER_MK2"; ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_PISTOL_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_SMG_MK2"; ComponentName = "COMPONENT_SMG_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_SMG_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }
            { WeaponName = "WEAPON_SNSPISTOL_MK2"; ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_INCENDIARY"; AmmoTypeName = "AMMO_PISTOL_INCENDIARY"; Effect = Mk2AmmoEffect.Incendiary }

            { WeaponName = "WEAPON_ASSAULTRIFLE_MK2"; ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_RIFLE_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_CARBINERIFLE_MK2"; ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_RIFLE_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_SPECIALCARBINE_MK2"; ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_RIFLE_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_BULLPUPRIFLE_MK2"; ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_RIFLE_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_COMBATMG_MK2"; ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_MG_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_HEAVYSNIPER_MK2"; ComponentName = "COMPONENT_HEAVYSNIPER_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_SNIPER_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_MARKSMANRIFLE_MK2"; ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_SNIPER_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_PISTOL_MK2"; ComponentName = "COMPONENT_PISTOL_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_PISTOL_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_REVOLVER_MK2"; ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_PISTOL_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_SMG_MK2"; ComponentName = "COMPONENT_SMG_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_SMG_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }
            { WeaponName = "WEAPON_SNSPISTOL_MK2"; ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_FMJ"; AmmoTypeName = "AMMO_PISTOL_FMJ"; Effect = Mk2AmmoEffect.FullMetalJacket }

            { WeaponName = "WEAPON_PISTOL_MK2"; ComponentName = "COMPONENT_PISTOL_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_PISTOL_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_SNSPISTOL_MK2"; ComponentName = "COMPONENT_SNSPISTOL_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_PISTOL_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_REVOLVER_MK2"; ComponentName = "COMPONENT_REVOLVER_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_PISTOL_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_SMG_MK2"; ComponentName = "COMPONENT_SMG_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_SMG_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_ASSAULTRIFLE_MK2"; ComponentName = "COMPONENT_ASSAULTRIFLE_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_RIFLE_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_CARBINERIFLE_MK2"; ComponentName = "COMPONENT_CARBINERIFLE_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_RIFLE_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_SPECIALCARBINE_MK2"; ComponentName = "COMPONENT_SPECIALCARBINE_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_RIFLE_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_BULLPUPRIFLE_MK2"; ComponentName = "COMPONENT_BULLPUPRIFLE_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_RIFLE_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_MARKSMANRIFLE_MK2"; ComponentName = "COMPONENT_MARKSMANRIFLE_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_SNIPER_TRACER"; Effect = Mk2AmmoEffect.Tracer }
            { WeaponName = "WEAPON_COMBATMG_MK2"; ComponentName = "COMPONENT_COMBATMG_MK2_CLIP_TRACER"; AmmoTypeName = "AMMO_MG_TRACER"; Effect = Mk2AmmoEffect.Tracer }
        |]

[<RequireQualifiedAccess>]
module internal RuleCatalog =
    [<Literal>]
    let ExplosiveAmmoExplosionType = 45
    [<Literal>]
    let ExplosiveAmmoShotgunExplosionType = 61

    let defaultCriticalShot = CriticalShotProfile.create 1 1000.0f
    let pumpShotgunMk2Hash = Hashing.joaat "WEAPON_PUMPSHOTGUN_MK2"

    let private tracerCadences =
        let values = Dictionary<uint32, TracerCadence>()
        let add name cadence = values[Hashing.joaat name] <- cadence
        add "WEAPON_PISTOL_MK2" (TracerCadence.create 1 3)
        add "WEAPON_SNSPISTOL_MK2" (TracerCadence.create 1 3)
        add "WEAPON_SMG_MK2" (TracerCadence.create 1 3)
        add "WEAPON_ASSAULTRIFLE_MK2" (TracerCadence.create 1 3)
        add "WEAPON_CARBINERIFLE_MK2" (TracerCadence.create 1 3)
        add "WEAPON_SPECIALCARBINE_MK2" (TracerCadence.create 1 3)
        add "WEAPON_BULLPUPRIFLE_MK2" (TracerCadence.create 1 3)
        add "WEAPON_MARKSMANRIFLE_MK2" (TracerCadence.create 0 3)
        add "WEAPON_COMBATMG_MK2" (TracerCadence.create 1 5)
        add "WEAPON_REVOLVER_MK2" (TracerCadence.create 1 0)
        values

    let private rules : AmmoRule array =
        let identities = HashSet<struct (uint32 * uint32 * uint32)>()
        OriginalNameCatalog.rules
        |> Array.choose (fun (original: OriginalNameRule) ->
            match Hashing.tryJoaat original.WeaponName,
                  Hashing.tryJoaat original.ComponentName,
                  Hashing.tryJoaat original.AmmoTypeName with
            | Some weaponHash, Some componentHash, Some ammoTypeHash ->
                let identity = struct (weaponHash, componentHash, ammoTypeHash)
                if identities.Add(identity) then
                    let rule : AmmoRule =
                        { WeaponName = original.WeaponName
                          ComponentName = original.ComponentName
                          AmmoTypeName = original.AmmoTypeName
                          WeaponHash = weaponHash
                          ComponentHash = componentHash
                          AmmoTypeHash = ammoTypeHash
                          Effect = original.Effect
                          CriticalShot = defaultCriticalShot }
                    Some rule
                else None
            | _ -> None)

    let private rulesByWeapon =
        let grouped = Dictionary<uint32, ResizeArray<AmmoRule>>()

        for rule in rules do
            match grouped.TryGetValue(rule.WeaponHash) with
            | true, bucket ->
                bucket.Add(rule)
            | false, _ ->
                let bucket = ResizeArray<AmmoRule>()
                bucket.Add(rule)
                grouped.Add(rule.WeaponHash, bucket)

        let indexed = Dictionary<uint32, AmmoRule array>(grouped.Count)
        for pair in grouped do
            indexed.Add(pair.Key, pair.Value.ToArray())
        indexed

    let ruleCount = rules.Length
    let originalCatalog = rules :> seq<AmmoRule>

    let tracerCadence weaponHash =
        match tracerCadences.TryGetValue(weaponHash) with
        | true, value -> value
        | _ -> TracerCadence.defaultValue

    let explosionType (rule: AmmoRule) =
        match rule.Effect with
        | Mk2AmmoEffect.Explosive when rule.WeaponHash = pumpShotgunMk2Hash -> ExplosiveAmmoShotgunExplosionType
        | Mk2AmmoEffect.Explosive -> ExplosiveAmmoExplosionType
        | _ -> -1

    let private resolveInstalledSpecialAmmo
        (game: GameAccess)
        (ped: Ped)
        (weaponHash: uint32)
        (weaponRules: AmmoRule array) =
        let installed =
            weaponRules
            |> Array.filter (fun candidate ->
                game.HasWeaponComponent(
                    ped,
                    weaponHash,
                    candidate.ComponentHash))

        match installed with
        | [||] ->
            None
        | [| only |] ->
            Some only
        | _ ->
            let active =
                installed
                |> Array.filter (fun candidate ->
                    game.IsWeaponComponentActive(
                        ped,
                        weaponHash,
                        candidate.ComponentHash))

            match active with
            | [| only |] ->
                Some only
            | values when values.Length > 1 ->
                let currentAmmoType = game.GetAmmoType(ped, weaponHash)
                values
                |> Array.tryFind (fun candidate ->
                    candidate.AmmoTypeHash = currentAmmoType)
            | _ ->
                let currentAmmoType = game.GetAmmoType(ped, weaponHash)
                installed
                |> Array.tryFind (fun candidate ->
                    candidate.AmmoTypeHash = currentAmmoType)

    let tryResolve (game: GameAccess) (ped: Ped) =
        let selectedWeapon = game.GetSelectedWeapon(ped)

        if selectedWeapon = 0u ||
           not (game.HasWeapon(ped, selectedWeapon)) then
            None
        else
            match rulesByWeapon.TryGetValue(selectedWeapon) with
            | false, _ ->
                None
            | true, weaponRules ->
                resolveInstalledSpecialAmmo
                    game
                    ped
                    selectedWeapon
                    weaponRules