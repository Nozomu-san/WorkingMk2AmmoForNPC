namespace WorkingMk2AmmoForNPC.Source

open System
open System.Numerics
open StandardGameOperations.Source

[<Sealed>]
type internal GameAccess(game: IStandardGameOperations) =
    do ArgumentNullException.ThrowIfNull(game)

    member _.IsSupported = game.Game.IsSupported
    member _.PlayerPed() = game.Peds.GetPlayerPed()
    member _.GetAllPeds() =
        game.Peds.GetAllPeds()
    member _.GetNearbyPeds(origin: Ped, maxAmount: int) =
        game.Peds.GetNearbyPeds(origin, maxAmount)
    member _.IsValidEntity(entity: Entity) = game.Entities.IsValid(entity)
    member _.IsValidPed(ped: Ped) = game.Entities.IsValid(ped)
    member _.IsValidVehicle(vehicle: Vehicle) = game.Entities.IsValid(vehicle)
    member _.IsLivingHumanPed(ped: Ped) = game.Peds.IsLivingHuman(ped)
    member _.IsDead(ped: Ped) = game.Peds.IsDead(ped)
    member _.IsShooting(ped: Ped) = game.Peds.IsShooting(ped)
    member _.IsPerformingMeleeAction(ped: Ped) =
        game.Peds.IsPerformingMeleeAction(ped)
    member _.IsReloading(ped: Ped) = game.Peds.IsReloading(ped)
    member _.ReadPedVitals(ped: Ped) = game.Peds.ReadVitals(ped)
    member _.GetCurrentVehicle(ped: Ped) = game.Peds.GetCurrentVehicle(ped)
    member _.GetPosition(ped: Ped) = game.Entities.GetPosition(ped)
    member _.IsPedInCover(ped: Ped) = game.Peds.IsInCover(ped)

    member _.TryRagdoll(ped: Ped) =
        game.Peds.TryRagdoll(ped)

    member _.IsOnFire(ped: Ped) = game.Peds.IsOnFire(ped)

    member _.StartFire(ped: Ped) =
        game.Peds.StartFire(ped) |> ignore

    member _.StopFire(ped: Ped) =
        game.Peds.StopFire(ped)

    member _.CanReceiveSyntheticDamage(ped: Ped) =
        game.Peds.CanReceiveSyntheticDamage(ped)

    member _.GetSelectedWeapon(ped: Ped) =
        game.Weapons.GetSelectedWeapon(ped)
    member _.HasWeapon(ped: Ped, weaponHash: uint32) =
        game.Weapons.HasWeapon(ped, weaponHash)
    member _.GetAmmoType(ped: Ped, weaponHash: uint32) =
        game.Weapons.GetAmmoType(ped, weaponHash)
    member _.HasWeaponComponent(
        ped: Ped,
        weaponHash: uint32,
        componentHash: uint32) =
        game.Weapons.HasComponent(ped, weaponHash, componentHash)

    member _.IsWeaponComponentActive(
        ped: Ped,
        weaponHash: uint32,
        componentHash: uint32) =
        game.Weapons.IsComponentActive(
            ped,
            weaponHash,
            componentHash)

    member _.TryGetAmmoInClip(ped: Ped, weaponHash: uint32) =
        let mutable ammo = 0
        if game.Weapons.TryGetAmmoInClip(ped, weaponHash, &ammo) then
            Some ammo
        else
            None

    member _.TryGetLastWeaponImpactCoordinate(shooter: Ped) =
        let mutable position = Vector3.Zero
        if game.Weapons.TryGetLastImpact(shooter, &position) then
            Some position
        else
            None

    member _.TryGetWeaponDamage(
        weaponHash: uint32,
        componentHash: uint32) =
        let mutable damage = 0.0f
        if game.Weapons.TryGetDamage(
               weaponHash,
               &damage,
               componentHash) then
            Some damage
        else
            None

    member this.TryGetWeaponDamage(weaponHash: uint32) =
        this.TryGetWeaponDamage(weaponHash, 0u)

    member _.TryGetCombatTarget(shooter: Ped) =
        let mutable target = Unchecked.defaultof<Ped>
        if game.Combat.TryGetTarget(shooter, &target) then
            Some target
        else
            None

    member _.ReadVehicleVitals(vehicle: Vehicle) =
        game.Damage.ReadVehicleVitals(vehicle)

    member _.CapturePedDamage(
        victim: Ped,
        previous: PedVitals) =
        game.Damage.CapturePedDamage(victim, previous)

    member _.CaptureVehicleDamage(
        victim: Vehicle,
        previous: VehicleVitals) =
        game.Damage.CaptureVehicleDamage(victim, previous)

    member _.WasDamagedByWeapon(
        victim: Ped,
        shooter: Ped,
        weaponHash: uint32) =
        game.Combat.WasDamagedByWeapon(
            game.Entities.AsEntity(victim),
            shooter,
            weaponHash)

    member _.WasDamagedByWeapon(
        victim: Vehicle,
        shooter: Ped,
        weaponHash: uint32) =
        game.Combat.WasDamagedByWeapon(
            game.Entities.AsEntity(victim),
            shooter,
            weaponHash)

    member _.ClearDamage(victim: Ped) =
        game.Combat.ClearDamageEvidence(
            game.Entities.AsEntity(victim))

    member _.ClearDamage(victim: Vehicle) =
        game.Combat.ClearDamageEvidence(
            game.Entities.AsEntity(victim))

    member _.TryResolveEngagement(shooter: Ped) =
        let mutable engagement = Unchecked.defaultof<CombatEngagement>
        if game.Combat.TryResolveEngagement(shooter, &engagement) then
            Some engagement
        else
            None

    member _.TryCaptureBaseline(engagement: CombatEngagement) =
        let mutable baseline = Unchecked.defaultof<EngagementBaseline>
        if game.Damage.TryCaptureBaseline(engagement, &baseline) then
            Some baseline
        else
            None

    member _.CaptureEngagementDamage(baseline: EngagementBaseline) =
        game.Damage.CaptureEngagementDamage(baseline)

    member _.WasDamagedByWeapon(
        engagement: CombatEngagement,
        weaponHash: uint32) =
        game.Combat.WasDamagedByWeapon(engagement, weaponHash)

    member _.ClearDamage(engagement: CombatEngagement) =
        game.Combat.ClearDamageEvidence(engagement)

    member _.AddOwnedExplosion(
        shooter: Ped,
        position: Vector3,
        explosionType: int) =
        game.Damage.AddOwnedExplosion(
            shooter,
            position,
            explosionType)

    member _.ApplyPedHealthDamage(victim: Ped, damageAmount: int) =
        game.Damage.ApplyPedHealthDamage(victim, damageAmount)

    member _.SetPedArmour(ped: Ped, amount: int) =
        game.Damage.SetPedArmour(ped, amount)
    member _.SetPedHealth(ped: Ped, amount: int) =
        game.Damage.SetPedHealth(ped, amount)
    member _.SetVehicleHealth(vehicle: Vehicle, amount: int) =
        game.Damage.SetVehicleHealth(vehicle, amount)
    member _.SetVehicleBodyHealth(vehicle: Vehicle, amount: single) =
        game.Damage.SetVehicleBodyHealth(vehicle, amount)
    member _.SetVehicleEngineHealth(vehicle: Vehicle, amount: single) =
        game.Damage.SetVehicleEngineHealth(vehicle, amount)

    member _.IsImpactOnTargetPath(
        shooter: Ped,
        target: Ped,
        impact: Vector3,
        maximumPerpendicularDistance: single,
        minimumPathFraction: single,
        maximumPathFraction: single) =
        game.Ballistics.IsImpactOnTargetPath(
            shooter,
            target,
            impact,
            maximumPerpendicularDistance,
            minimumPathFraction,
            maximumPathFraction)