namespace WorkingMk2AmmoForNPC.Source

open System
open System.Numerics
open StandardGameOperations.Source

[<Sealed>]
type internal GameAccess(game: IStandardGameOperations) =
    do ArgumentNullException.ThrowIfNull(game)

    member _.IsValidPed(ped: Ped) = game.Entities.IsValid(ped)
    member _.IsValidVehicle(vehicle: Vehicle) = game.Entities.IsValid(vehicle)
    member _.IsHuman(ped: Ped) = game.Peds.IsHuman(ped)
    member _.IsDead(ped: Ped) = game.Peds.IsDead(ped)
    member _.IsPedInCover(ped: Ped) = game.Peds.IsInCover(ped)
    member _.GetCurrentVehicle(ped: Ped) = game.Peds.GetCurrentVehicle(ped)
    member _.RequestLeaveVehicle(ped: Ped) = game.Peds.RequestLeaveVehicle(ped)
    member _.ReadPedVitals(ped: Ped) = game.Peds.ReadVitals(ped)
    member _.ReadInvincibility(ped: Ped) = game.Peds.ReadInvincibility(ped)
    member _.SetPedArmour(ped: Ped, amount: int) = game.Damage.SetPedArmour(ped, amount)
    member _.SetPedHealth(ped: Ped, amount: int) = game.Damage.SetPedHealth(ped, amount)
    member _.TryRagdoll(ped: Ped) = game.Peds.TryRagdoll(ped)
    member _.IsReloading(ped: Ped) = game.Peds.IsReloading(ped)
    member _.GetPosition(ped: Ped) = game.Entities.GetPosition(ped)

    member _.IsNpcShooter(ped: Ped) =
        let player = game.Peds.GetPlayerPed()
        ped.Value <> 0 &&
        ped.Value <> player.Value &&
        game.Peds.IsLivingHuman(ped)

    member _.IsOnFire(ped: Ped) = game.Peds.IsOnFire(ped)
    member _.StartFire(ped: Ped) = game.Peds.StartFire(ped) |> ignore
    member _.StopFire(ped: Ped) = game.Peds.StopFire(ped)
    member _.CanReceiveSyntheticDamage(ped: Ped) =
        game.Peds.CanReceiveSyntheticDamage(ped)

    member _.HasComponent(ped: Ped, weaponHash: uint32, componentHash: uint32) =
        game.Weapons.HasComponent(ped, weaponHash, componentHash)

    member _.IsComponentActive(ped: Ped, weaponHash: uint32, componentHash: uint32) =
        game.Weapons.IsComponentActive(ped, weaponHash, componentHash)

    member _.TryGetAmmoInClip(ped: Ped, weaponHash: uint32) =
        let mutable ammo = 0
        if game.Weapons.TryGetAmmoInClip(ped, weaponHash, &ammo) then
            Some ammo
        else
            None

    member _.GetMaxAmmoInClip(ped: Ped, weaponHash: uint32) =
        game.Weapons.GetMaxAmmoInClip(ped, weaponHash)

    member _.TryGetLastImpact(ped: Ped) =
        let mutable position = Vector3.Zero
        if game.Weapons.TryGetLastImpact(ped, &position) then
            Some position
        else
            None

    member _.TryGetWeaponDamage(weaponHash: uint32, componentHash: uint32) =
        let mutable damage = 0f
        if game.Weapons.TryGetDamage(weaponHash, &damage, componentHash) then
            Some damage
        else
            None

    member _.TryResolveEngagement(shooter: Ped) =
        let mutable engagement = Unchecked.defaultof<CombatEngagement>
        if game.Combat.TryResolveEngagement(shooter, &engagement) then
            Some engagement
        else
            None

    member _.AddOwnedExplosion(
        shooter: Ped,
        position: Vector3,
        explosionType: int) =
        game.Damage.AddOwnedExplosion(shooter, position, explosionType)

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