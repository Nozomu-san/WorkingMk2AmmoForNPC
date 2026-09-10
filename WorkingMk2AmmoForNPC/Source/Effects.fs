namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic
open StandardGameOperations.Source

[<Sealed>]
type private FireState(victim: Ped) =
    member _.Victim = victim
    member val ActiveSince : TimeSpan option = None with get, set
    member val LastRequestAt = TimeSpan.Zero with get, set

[<Sealed>]
type internal Mk2Effects(game: GameAccess) =
    do ArgumentNullException.ThrowIfNull(game)

    let incendiaryFireDuration = TimeSpan.FromMilliseconds 3000.0
    let fireRequestInterval = TimeSpan.FromMilliseconds 250.0
    let fires = Dictionary<int, FireState>()
    let fireRemovalBuffer = ResizeArray<int>()

    let isLiveVictim (ped: Ped) =
        game.IsValidPed(ped) && not (game.IsDead(ped))

    let canReceiveSecondaryEffect (ped: Ped) =
        isLiveVictim ped && game.CanReceiveSyntheticDamage(ped)

    let toDamageUnits (value: single) =
        if not (Single.IsFinite(value)) || value <= 0f then
            0
        else
            let rounded = Math.Round(double value, MidpointRounding.AwayFromZero)
            if rounded >= double Int32.MaxValue then Int32.MaxValue
            else max 0 (int rounded)

    let saturatingAdd current delta =
        if delta <= 0 then current
        elif current > Int32.MaxValue - delta then Int32.MaxValue
        else current + delta

    let setHealthIfChanged victim current target =
        let clamped = max 0 target
        if clamped = current then
            false
        else
            game.SetPedHealth(victim, clamped)
            true

    let setArmourIfChanged victim current target =
        let clamped = max 0 target
        if clamped = current then
            false
        else
            game.SetPedArmour(victim, clamped)
            true

    let applyAdditionalDamage victim amount =
        if amount <= 0 then
            false
        else
            let vitals = game.ReadPedVitals(victim)
            let armourLoss = min vitals.Armour amount
            let remaining = amount - armourLoss
            let mutable changed = false
            if armourLoss > 0 then
                game.SetPedArmour(victim, vitals.Armour - armourLoss)
                changed <- true
            if remaining > 0 then
                let targetHealth = max 0 (vitals.Health - remaining)
                if targetHealth < vitals.Health then
                    game.SetPedHealth(victim, targetHealth)
                    changed <- true
            changed

    let applyHollowPoint victim (observation: PedDamageObservation) =
        let damage = observation.ObservedDamage
        if damage <= 0 || observation.ArmourLoss > 0 then
            false
        else
            let extra = damage
            setHealthIfChanged
                victim
                observation.After.Health
                (observation.After.Health - extra)

    let applyArmorPiercing victim (observation: PedDamageObservation) =
        let damage = observation.ObservedDamage
        if damage <= 0 || observation.Before.Armour <= 0 then
            false
        else
            let desiredArmourLoss =
                min
                    observation.Before.Armour
                    (toDamageUnits (single damage * 0.20f))
            let armourRestore =
                max 0 (observation.ArmourLoss - desiredArmourLoss)
            let targetArmour =
                min
                    observation.Before.Armour
                    (observation.After.Armour + armourRestore)
            let targetHealth = max 0 (observation.Before.Health - damage)
            let armourChanged =
                setArmourIfChanged victim observation.After.Armour targetArmour
            let healthChanged =
                setHealthIfChanged victim observation.After.Health targetHealth
            armourChanged || healthChanged

    let restoreIncendiaryFraction victim (observation: PedDamageObservation) =
        let armourRestore = toDamageUnits (single observation.ArmourLoss * 0.10f)
        let healthRestore = toDamageUnits (single observation.HealthLoss * 0.10f)
        let targetArmour =
            min observation.Before.Armour (observation.After.Armour + armourRestore)
        let targetHealth =
            min observation.Before.Health (observation.After.Health + healthRestore)
        let armourChanged =
            setArmourIfChanged victim observation.After.Armour targetArmour
        let healthChanged =
            setHealthIfChanged victim observation.After.Health targetHealth
        armourChanged || healthChanged

    let beginManagedFire (state: FireState) now =
        game.StartFire(state.Victim)
        state.ActiveSince <- Some now
        state.LastRequestAt <- now

    let tryIgniteNew (victim: Ped) now =
        if not (canReceiveSecondaryEffect victim) ||
           fires.ContainsKey(victim.Value) ||
           game.IsOnFire(victim) then
            false
        else
            let state = FireState(victim)
            if not (fires.TryAdd(victim.Value, state)) then
                false
            else
                let vehicle = game.GetCurrentVehicle(victim)
                if game.IsValidVehicle(vehicle) then
                    game.RequestLeaveVehicle(victim) |> ignore
                else
                    beginManagedFire state now
                true

    member _.TryApplyIncendiaryCoverIgnition(
        victim: Ped,
        now: TimeSpan,
        shooterState: ShooterState) =
        if not (canReceiveSecondaryEffect victim) ||
           fires.ContainsKey(victim.Value) ||
           game.IsOnFire(victim) then
            false
        elif shooterState.ShouldIgniteIncendiary(victim) then
            tryIgniteNew victim now
        else
            false

    member _.ApplyDirectPedDamage(
        rule: ComponentRule,
        balance: Mk2BalanceSettings,
        ridiculousMode: bool,
        tracerBoost: bool,
        victim: Ped,
        observation: PedDamageObservation,
        now: TimeSpan,
        shooterState: ShooterState) =
        if not (isLiveVictim victim) ||
           not (game.CanReceiveSyntheticDamage(victim)) ||
           observation.ObservedDamage <= 0 then
            false
        else
            match rule.Effect with
            | Mk2AmmoEffect.HollowPoint when balance.HollowPointDamageHandlerEnabled ->
                applyHollowPoint victim observation
            | Mk2AmmoEffect.ArmorPiercing when balance.ArmorPiercingDamageHandlerEnabled ->
                applyArmorPiercing victim observation
            | Mk2AmmoEffect.Incendiary when balance.IncendiaryDamageHandlerEnabled ->
                let restored = restoreIncendiaryFraction victim observation
                let ignited =
                    if shooterState.ShouldIgniteIncendiary(victim) then
                        tryIgniteNew victim now
                    else
                        false
                restored || ignited
            | Mk2AmmoEffect.Tracer
                when ridiculousMode &&
                     balance.TracerDamageHandlerEnabled &&
                     tracerBoost ->
                let multiplier = max 1f balance.TracerDamageMultiplier
                let extra =
                    toDamageUnits (single observation.ObservedDamage * (multiplier - 1f))
                applyAdditionalDamage victim extra
            | _ -> false

    member _.Advance(now: TimeSpan) =
        if fires.Count > 0 then
            fireRemovalBuffer.Clear()
            for pair in fires do
                let state = pair.Value
                if not (game.IsValidPed(state.Victim)) ||
                   game.IsDead(state.Victim) ||
                   not (game.CanReceiveSyntheticDamage(state.Victim)) then
                    if game.IsValidPed(state.Victim) then
                        game.StopFire(state.Victim)
                    fireRemovalBuffer.Add(pair.Key)
                else
                    match state.ActiveSince with
                    | None ->
                        let vehicle = game.GetCurrentVehicle(state.Victim)
                        if game.IsValidVehicle(vehicle) then
                            if now - state.LastRequestAt >= fireRequestInterval then
                                game.RequestLeaveVehicle(state.Victim) |> ignore
                                state.LastRequestAt <- now
                        else
                            beginManagedFire state now
                    | Some activeSince ->
                        if now - activeSince >= incendiaryFireDuration then
                            game.StopFire(state.Victim)
                            fireRemovalBuffer.Add(pair.Key)
                        elif game.IsOnFire(state.Victim) then
                            ()
                        elif now - state.LastRequestAt >= fireRequestInterval then
                            game.StartFire(state.Victim)
                            state.LastRequestAt <- now
            for handle in fireRemovalBuffer do
                fires.Remove(handle) |> ignore

    member _.StopAll() =
        for state in fires.Values do
            if game.IsValidPed(state.Victim) then game.StopFire(state.Victim)
        fires.Clear()
