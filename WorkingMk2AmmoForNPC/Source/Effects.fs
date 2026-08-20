namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic
open StandardGameOperations.Source

[<Sealed>]
type private FireState(victim: Ped, startedAt: TimeSpan, lastRequestAt: TimeSpan) =
    member _.Victim = victim
    member _.StartedAt = startedAt
    member val LastRequestAt = lastRequestAt with get, set
    member val RequestCount = 0 with get, set
    member val ObservedOnFire = false with get, set

[<Sealed>]
type internal Mk2Effects(game: GameAccess) =
    do ArgumentNullException.ThrowIfNull(game)

    let fullMetalJacketVehicleMultiplier = 2.0f
    let tracerHiddenRoundDamageMultiplier = 2.0f
    let incendiaryDamageMultiplier = 0.9f
    let incendiaryVehicleBodyDamageFraction = 0.9f
    let incendiaryVehicleEngineTransferFraction = 0.1f
    let incendiaryVehicleEngineTransferMultiplier = 4.0f
    let armorPiercingVehicleHealthTransferFraction = 0.6f
    let explosivePedDamageMultiplier = 3.0f
    let explosiveInVehicleDamageFraction = 0.4f
    let incendiarySubsequentIgnitionChancePercent = 5
    let incendiaryFireDuration = TimeSpan.FromMilliseconds 3000.0
    let incendiaryRefreshInterval = TimeSpan.FromMilliseconds 250.0
    let fires = Dictionary<int, FireState>()
    let fireRemovalBuffer = ResizeArray<int>()

    let roundAway (value: double) =
        if not (Double.IsFinite(value)) || value <= 0.0 then 0
        elif value >= double Int32.MaxValue then Int32.MaxValue
        else int (Math.Round(value, MidpointRounding.AwayFromZero))

    let divideRoundUp numerator denominator =
        if denominator <= 0 then 0 else (numerator + denominator - 1) / denominator

    let applyTotalPedDamageMultiplier (snapshot: PedDamageSnapshot) multiplier =
        if not snapshot.HasDamage || not (Single.IsFinite(multiplier)) || multiplier <= 1.0f then
            false
        else
            let acceptedDamage = roundAway (double snapshot.TotalLoss * double multiplier)
            let targetArmour, targetHealth =
                if snapshot.PreviousArmour >= acceptedDamage then
                    snapshot.PreviousArmour - acceptedDamage, snapshot.PreviousHealth
                else
                    0, max 0 (snapshot.PreviousHealth - (acceptedDamage - snapshot.PreviousArmour))

            let mutable changed = false
            if targetArmour < snapshot.CurrentArmour then
                game.SetPedArmour(snapshot.Victim, targetArmour)
                changed <- true
            if targetHealth < snapshot.CurrentHealth then
                game.SetPedHealth(snapshot.Victim, targetHealth)
                changed <- true
            changed

    let requestManagedFire (state: FireState) now =
        game.StartFire(state.Victim)
        state.LastRequestAt <- now
        state.RequestCount <- state.RequestCount + 1

    let tryIgniteNew (victim: Ped) now =
        if not (game.IsLivingHumanPed(victim)) || fires.ContainsKey(victim.Value) || game.IsOnFire(victim) then
            false
        else
            let state = FireState(victim, now, now - incendiaryRefreshInterval)
            if not (fires.TryAdd(victim.Value, state)) then false
            else
                requestManagedFire state now
                true

    let restoreIncendiaryDamage (snapshot: PedDamageSnapshot) =
        let acceptedDamage = roundAway (double snapshot.TotalLoss * double incendiaryDamageMultiplier)
        let targetArmour, targetHealth =
            if snapshot.PreviousArmour >= acceptedDamage then
                snapshot.PreviousArmour - acceptedDamage, snapshot.PreviousHealth
            else
                0, max 0 (snapshot.PreviousHealth - (acceptedDamage - snapshot.PreviousArmour))

        let mutable changed = false
        if targetArmour > snapshot.CurrentArmour then
            game.SetPedArmour(snapshot.Victim, targetArmour)
            changed <- true
        if targetHealth > snapshot.CurrentHealth then
            game.SetPedHealth(snapshot.Victim, targetHealth)
            changed <- true
        changed

    member _.ApplyHollowPoint(snapshot: PedDamageSnapshot) =
        if not snapshot.HasDamage || snapshot.HealthLoss <= 0 then false
        else
            game.ApplyPedHealthDamage(snapshot.Victim, snapshot.HealthLoss)
            true

    member _.ApplyArmorPiercing(snapshot: PedDamageSnapshot) =
        if not snapshot.HasDamage then false
        else
            let mutable changed = false
            if snapshot.ArmourLoss > 0 then
                game.ApplyPedHealthDamage(snapshot.Victim, snapshot.ArmourLoss)
                changed <- true
            let targetArmour = max 0 (snapshot.PreviousArmour - divideRoundUp snapshot.TotalLoss 5)
            if targetArmour > snapshot.CurrentArmour then
                game.SetPedArmour(snapshot.Victim, targetArmour)
                changed <- true
            changed

    member _.ApplyIncendiary(snapshot: PedDamageSnapshot, now: TimeSpan, shooterState: ShooterState) =
        if not snapshot.HasDamage then false
        else
            let changed = restoreIncendiaryDamage snapshot
            if not (game.IsLivingHumanPed(snapshot.Victim)) || fires.ContainsKey(snapshot.Victim.Value) || game.IsOnFire(snapshot.Victim) then
                changed
            elif not (shooterState.ShouldIgniteIncendiary(incendiarySubsequentIgnitionChancePercent)) then
                changed
            else
                tryIgniteNew snapshot.Victim now || changed

    member _.ApplyIncendiaryToVehicle(snapshot: VehicleDamageSnapshot) =
        if snapshot.BodyHealthLoss <= 0.0f ||
           not (Single.IsFinite(snapshot.BodyHealthLoss)) ||
           not (Single.IsFinite(snapshot.Current.BodyHealth)) ||
           not (Single.IsFinite(snapshot.Current.EngineHealth)) then false
        else
            let retainedBodyLoss = snapshot.BodyHealthLoss * incendiaryVehicleBodyDamageFraction
            let convertedBodyLoss = snapshot.BodyHealthLoss * incendiaryVehicleEngineTransferFraction
            let targetBodyHealth = snapshot.Previous.BodyHealth - retainedBodyLoss
            let additionalEngineDamage = convertedBodyLoss * incendiaryVehicleEngineTransferMultiplier
            game.SetVehicleBodyHealth(snapshot.Victim, targetBodyHealth)
            game.SetVehicleEngineHealth(snapshot.Victim, snapshot.Current.EngineHealth - additionalEngineDamage)
            true

    member _.ApplyTransferredArmorPiercingDamage(victim: Ped, vehicleBodyHealthLoss: single) =
        if not (game.IsLivingHumanPed(victim)) ||
           not (Single.IsFinite(vehicleBodyHealthLoss)) || vehicleBodyHealthLoss <= 0.0f ||
           not (game.CanReceiveSyntheticDamage(victim)) then false
        else
            let amount = roundAway (double vehicleBodyHealthLoss * double armorPiercingVehicleHealthTransferFraction)
            if amount <= 0 then false
            else
                game.ApplyPedHealthDamage(victim, amount)
                true

    member _.ApplyExplosiveTotalDamageMultiplier(snapshot: PedDamageSnapshot, victimWasInVehicle: bool) =
        if not (game.CanReceiveSyntheticDamage(snapshot.Victim)) then false
        else
            let multiplier = explosivePedDamageMultiplier * (if victimWasInVehicle then explosiveInVehicleDamageFraction else 1.0f)
            applyTotalPedDamageMultiplier snapshot multiplier

    member _.ApplyExplosiveNominalDamage(victim: Ped, nominalDamage: single, victimWasInVehicle: bool) =
        if not (game.IsLivingHumanPed(victim)) ||
           not (Single.IsFinite(nominalDamage)) || nominalDamage <= 0.0f ||
           not (game.CanReceiveSyntheticDamage(victim)) then false
        else
            let scaled = double nominalDamage * double explosivePedDamageMultiplier * (if victimWasInVehicle then double explosiveInVehicleDamageFraction else 1.0)
            let amount = roundAway scaled
            if amount <= 0 then false
            else
                let current = game.ReadPedVitals(victim)
                let targetArmour, targetHealth =
                    if current.Armour >= amount then current.Armour - amount, current.Health
                    else 0, max 0 (current.Health - (amount - current.Armour))
                let mutable changed = false
                if targetArmour < current.Armour then
                    game.SetPedArmour(victim, targetArmour)
                    changed <- true
                if targetHealth < current.Health then
                    game.SetPedHealth(victim, targetHealth)
                    changed <- true
                changed

    member _.TryApplyIncendiaryCoverIgnition(victim: Ped, now: TimeSpan, shooterState: ShooterState) =
        if not (game.IsLivingHumanPed(victim)) || fires.ContainsKey(victim.Value) || game.IsOnFire(victim) then false
        elif not (shooterState.ShouldIgniteIncendiary(incendiarySubsequentIgnitionChancePercent)) then false
        else tryIgniteNew victim now

    member _.ApplyFullMetalJacket(snapshot: VehicleDamageSnapshot) =
        if not snapshot.HasDamage then
            Unchecked.defaultof<FullMetalJacketVehicleEffect>
        else
            let additionalMultiplier = fullMetalJacketVehicleMultiplier - 1.0f
            let mutable changed = false
            if snapshot.BodyHealthLoss > 0.0f then
                game.SetVehicleBodyHealth(snapshot.Victim, max 0.0f (snapshot.Current.BodyHealth - snapshot.BodyHealthLoss * additionalMultiplier))
                changed <- true
            if snapshot.EngineHealthLoss > 0.0f then
                game.SetVehicleEngineHealth(snapshot.Victim, snapshot.Current.EngineHealth - snapshot.EngineHealthLoss * additionalMultiplier)
                changed <- true
            if snapshot.BodyHealthLoss <= 0.0f && snapshot.EngineHealthLoss <= 0.0f && snapshot.EntityHealthLoss > 0 then
                game.SetVehicleHealth(snapshot.Victim, max 0 (snapshot.Current.EntityHealth - snapshot.EntityHealthLoss))
                changed <- true
            { Applied = changed
              TransferableDirectHealthLoss = snapshot.TransferableDirectHealthLoss }

    member _.ApplyFullMetalJacketToPed(snapshot: PedDamageSnapshot) =
        game.CanReceiveSyntheticDamage(snapshot.Victim) &&
        applyTotalPedDamageMultiplier snapshot fullMetalJacketVehicleMultiplier

    member _.ApplyTransferredFullMetalJacketDamage(victim: Ped, directDamageBasis: single) =
        if not (game.IsLivingHumanPed(victim)) ||
           not (Single.IsFinite(directDamageBasis)) || directDamageBasis <= 0.0f ||
           not (game.CanReceiveSyntheticDamage(victim)) then false
        else
            let amount = roundAway (double directDamageBasis * double fullMetalJacketVehicleMultiplier)
            if amount <= 0 then false
            else
                game.ApplyPedHealthDamage(victim, amount)
                true

    member _.ApplyTracerToPed(snapshot: PedDamageSnapshot, shouldDoubleDamage: bool) =
        shouldDoubleDamage &&
        game.CanReceiveSyntheticDamage(snapshot.Victim) &&
        applyTotalPedDamageMultiplier snapshot tracerHiddenRoundDamageMultiplier

    member _.ApplyTracerToVehicle(snapshot: VehicleDamageSnapshot, shouldDoubleDamage: bool) =
        if not shouldDoubleDamage || not snapshot.HasDamage then false
        else
            let additionalMultiplier = tracerHiddenRoundDamageMultiplier - 1.0f
            let mutable changed = false
            if snapshot.BodyHealthLoss > 0.0f then
                game.SetVehicleBodyHealth(snapshot.Victim, max 0.0f (snapshot.Current.BodyHealth - snapshot.BodyHealthLoss * additionalMultiplier))
                changed <- true
            if snapshot.EngineHealthLoss > 0.0f then
                game.SetVehicleEngineHealth(snapshot.Victim, snapshot.Current.EngineHealth - snapshot.EngineHealthLoss * additionalMultiplier)
                changed <- true
            if snapshot.BodyHealthLoss <= 0.0f && snapshot.EngineHealthLoss <= 0.0f && snapshot.EntityHealthLoss > 0 then
                game.SetVehicleHealth(snapshot.Victim, max 0 (snapshot.Current.EntityHealth - snapshot.EntityHealthLoss))
                changed <- true
            changed

    member _.ApplyCriticalShot(snapshot: PedDamageSnapshot, profile: CriticalShotProfile, shooterState: ShooterState) =
        if not snapshot.HasDamage ||
           not (game.IsLivingHumanPed(snapshot.Victim)) ||
           not (shooterState.ShouldApplyCriticalShot(profile)) then false
        else
            let extraDamage = double snapshot.TotalLoss * (double profile.DamageMultiplier - 1.0)
            let amount = roundAway extraDamage
            if amount <= 0 then false
            else
                game.ApplyPedHealthDamage(snapshot.Victim, amount)
                true

    member _.Advance(now: TimeSpan) =
        if fires.Count > 0 then
            fireRemovalBuffer.Clear()
            for pair in fires do
                let state = pair.Value
                if not (game.IsValidPed(state.Victim)) || game.IsDead(state.Victim) then
                    if game.IsValidPed(state.Victim) then game.StopFire(state.Victim)
                    fireRemovalBuffer.Add(pair.Key)
                elif now - state.StartedAt >= incendiaryFireDuration then
                    game.StopFire(state.Victim)
                    fireRemovalBuffer.Add(pair.Key)
                elif game.IsOnFire(state.Victim) then
                    state.ObservedOnFire <- true
                elif now - state.LastRequestAt >= incendiaryRefreshInterval then
                    requestManagedFire state now
            for handle in fireRemovalBuffer do fires.Remove(handle) |> ignore

    member _.StopAll() =
        for state in fires.Values do
            if game.IsValidPed(state.Victim) then game.StopFire(state.Victim)
        fires.Clear()