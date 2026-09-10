namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic
open System.Numerics
open StandardGameOperations.Source

[<Sealed>]
type private TracerRuntimeState() =
    member val Initialized = false with get, set
    member val LastClipAmmo : int option = None with get, set
    member val ShotNumber = 0 with get, set
    member val Quota = MagazineQuotaState()

[<Sealed>]
type internal ShooterState(balance: Mk2BalanceSettings) =
    let incendiaryStates = Dictionary<int, WeibullIgnitionState>()
    let tracerStates = Dictionary<uint32, TracerRuntimeState>()

    let getIgnitionState victimHandle =
        match incendiaryStates.TryGetValue(victimHandle) with
        | true, state -> state
        | _ ->
            let state = WeibullIgnitionState()
            incendiaryStates.Add(victimHandle, state)
            state

    let getTracerState weaponHash =
        match tracerStates.TryGetValue(weaponHash) with
        | true, state -> state
        | _ ->
            let state = TracerRuntimeState()
            tracerStates.Add(weaponHash, state)
            state

    member _.ShouldIgniteIncendiary(victim: Ped) =
        if victim.Value = 0 then
            false
        else
            match balance.IncendiaryIgnitionMode with
            | IncendiaryIgnitionMode.Always -> true
            | IncendiaryIgnitionMode.Never -> false
            | IncendiaryIgnitionMode.Weibull ->
                let ignition = getIgnitionState victim.Value
                ignition.TryNext(balance.IncendiaryIgnition)
            | _ -> false

    member _.ResolveTracerBoost(
        weaponHash: uint32,
        cadence: TracerCadence,
        clipAmmo: int option,
        clipSize: int,
        isReloading: bool) =
        let state = getTracerState weaponHash
        let reloadDetected =
            isReloading ||
            match state.LastClipAmmo, clipAmmo with
            | Some previous, Some current when current > previous -> true
            | _ -> false

        if not state.Initialized || reloadDetected then
            state.Initialized <- true
            state.ShotNumber <- 0
            state.Quota.Reset(max 1 clipSize, balance.HeavyRevolverTracerBoostRate)

        state.ShotNumber <- state.ShotNumber + 1
        match clipAmmo with
        | Some value -> state.LastClipAmmo <- Some value
        | None -> ()

        if cadence.UsesFixedQuota then
            match balance.HeavyRevolverTracerBoostMode with
            | HeavyRevolverTracerBoostMode.Always -> true
            | HeavyRevolverTracerBoostMode.Never -> false
            | HeavyRevolverTracerBoostMode.MagazineQuota -> state.Quota.TakeNext()
            | _ -> false
        else
            not (TracerCadence.isTracerShot state.ShotNumber cadence)

    member _.Clear() =
        incendiaryStates.Clear()
        tracerStates.Clear()

[<Sealed>]
type internal ShooterRuntimeState(
    ped: Ped,
    balance: Mk2BalanceSettings,
    firstSeenAt: TimeSpan) =
    member _.Ped = ped
    member _.Effects = ShooterState(balance)
    member val LastSeenAt = firstSeenAt with get, set

[<Sealed>]
type internal AimContext(
    sequence: uint64,
    shooterAddress: uint64,
    shooter: Ped,
    victimAddress: uint64,
    victim: Ped) =
    member _.Sequence = sequence
    member _.ShooterAddress = shooterAddress
    member _.Shooter = shooter
    member _.VictimAddress = victimAddress
    member _.Victim = victim

[<Sealed>]
type internal VictimRuntimeState(
    victimAddress: uint64,
    victim: Ped,
    lastKnown: PedVitals,
    sequence: uint64) =
    member val VictimAddress = victimAddress with get, set
    member _.Victim = victim
    member val LastKnown = lastKnown with get, set
    member val Sequence = sequence with get, set
    member val Revision = 0UL with get, set

[<Struct>]
type internal PedDamageObservation =
    {
        Before: PedVitals
        After: PedVitals
        ArmourLoss: int
        HealthLoss: int
        ObservedDamage: int
    }

[<Sealed>]
type internal ShotContext(
    sequence: uint64,
    shooterAddress: uint64,
    shooter: Ped,
    weaponHash: uint32,
    rule: ComponentRule,
    tracerBoost: bool,
    expiresAt: TimeSpan,
    engagement: CombatEngagement option) =
    member _.Sequence = sequence
    member _.ShooterAddress = shooterAddress
    member _.Shooter = shooter
    member _.WeaponHash = weaponHash
    member _.Rule = rule
    member _.TracerBoost = tracerBoost
    member _.ExpiresAt = expiresAt
    member _.Engagement = engagement
    member val ImpactApplied = false with get, set
    member val DamageApplied = false with get, set
    member val ImpactPosition : Vector3 option = None with get, set
