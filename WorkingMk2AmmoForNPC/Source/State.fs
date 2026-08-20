namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic
open System.Numerics
open StandardGameOperations.Source

[<Sealed>]
type private TracerMagazineState() =
    member val HasClipSample = false with get, set
    member val LastClipAmmo = 0 with get, set
    member val ShotNumber = 0 with get, set

[<Sealed>]
type internal ShooterState() =
    let minimumShotInterval = TimeSpan.FromMilliseconds 65.0
    let pendingLifetime = TimeSpan.FromMilliseconds 650.0
    let probeInterval = TimeSpan.FromMilliseconds 33.0
    let tracerSampleInterval = TimeSpan.FromMilliseconds 33.0
    let duplicateInterval = TimeSpan.FromMilliseconds 100.0
    let duplicateDistanceSquared = 0.01f
    let probability = Random(Random.Shared.Next())
    let tracerMagazines = Dictionary<uint32, TracerMagazineState>()

    let mutable pending = false
    let mutable hasShotTime = false
    let mutable lastShotAt = TimeSpan.Zero
    let mutable pendingUntil = TimeSpan.Zero
    let mutable nextProbeAt = TimeSpan.Zero
    let mutable nextTracerSampleAt = TimeSpan.Zero
    let mutable hasFmjFallbackImpact = false
    let mutable lastFmjFallbackWeaponHash = 0u
    let mutable lastFmjFallbackTargetHandle = 0
    let mutable lastFmjFallbackImpact = Vector3.Zero
    let mutable lastFmjFallbackAt = TimeSpan.Zero
    let mutable incendiaryFirstIgnitionAvailable = true
    let mutable hasIncendiaryCoverImpact = false
    let mutable lastIncendiaryCoverWeaponHash = 0u
    let mutable lastIncendiaryCoverTargetHandle = 0
    let mutable lastIncendiaryCoverImpact = Vector3.Zero
    let mutable lastIncendiaryCoverAt = TimeSpan.Zero
    let mutable tracerPendingDecision = false
    let mutable tracerPendingDoubleDamage = false

    let getTracerMagazine weaponHash =
        match tracerMagazines.TryGetValue(weaponHash) with
        | true, magazine -> magazine
        | _ ->
            let magazine = TracerMagazineState()
            tracerMagazines.Add(weaponHash, magazine)
            magazine

    let openPendingWindow now =
        pending <- true
        pendingUntil <- now + pendingLifetime
        nextProbeAt <- now

    let shouldDoubleTracerDamage (cadence: TracerCadence) (oneBasedShotNumber: int) =
        if cadence.IsEveryShotRandomDoubleException then
            probability.Next(2) = 0
        else
            not (TracerCadence.isTracerShot oneBasedShotNumber cadence)

    member _.CurrentTracerShouldDoubleDamage =
        tracerPendingDecision && tracerPendingDoubleDamage

    member _.BeginShot(now: TimeSpan) =
        if hasShotTime && now - lastShotAt < minimumShotInterval then
            false
        else
            hasShotTime <- true
            lastShotAt <- now
            openPendingWindow now
            true

    member _.PrimeTracerMagazine(weaponHash: uint32, clipAmmo: int, isReloading: bool) =
        if weaponHash <> 0u then
            let magazine = getTracerMagazine weaponHash
            magazine.HasClipSample <- true
            magazine.LastClipAmmo <- max 0 clipAmmo
            if isReloading then magazine.ShotNumber <- 0

    member _.ObserveTracerMagazine(
        weaponHash: uint32,
        cadence: TracerCadence,
        clipAmmoValue: int,
        isReloading: bool,
        now: TimeSpan) =
        if weaponHash <> 0u then
            let magazine = getTracerMagazine weaponHash
            let clipAmmo = max 0 clipAmmoValue

            if not magazine.HasClipSample then
                magazine.HasClipSample <- true
                magazine.LastClipAmmo <- clipAmmo
                if isReloading then magazine.ShotNumber <- 0
            elif clipAmmo > magazine.LastClipAmmo then
                magazine.ShotNumber <- 0
                magazine.LastClipAmmo <- clipAmmo
            else
                let consumedRounds = magazine.LastClipAmmo - clipAmmo
                magazine.LastClipAmmo <- clipAmmo
                if consumedRounds <= 0 then
                    if isReloading then magazine.ShotNumber <- 0
                else
                    let mutable batchHasDecision = false
                    let mutable batchDoubleDamage = false
                    let mutable mixedBatch = false
                    for _index in 0 .. consumedRounds - 1 do
                        magazine.ShotNumber <- magazine.ShotNumber + 1
                        let shouldDouble = shouldDoubleTracerDamage cadence magazine.ShotNumber
                        if not batchHasDecision then
                            batchHasDecision <- true
                            batchDoubleDamage <- shouldDouble
                        elif batchDoubleDamage <> shouldDouble then
                            mixedBatch <- true

                    tracerPendingDecision <- batchHasDecision
                    tracerPendingDoubleDamage <- batchHasDecision && not mixedBatch && batchDoubleDamage
                    if isReloading then magazine.ShotNumber <- 0
                    hasShotTime <- true
                    lastShotAt <- now
                    openPendingWindow now

    member _.ResetTracerMagazine(weaponHash: uint32, observedClipAmmo: int option) =
        if weaponHash <> 0u then
            let magazine = getTracerMagazine weaponHash
            magazine.ShotNumber <- 0
            match observedClipAmmo with
            | Some clipAmmo ->
                magazine.HasClipSample <- true
                magazine.LastClipAmmo <- max 0 clipAmmo
            | None -> ()

    member _.MarkTracerFallbackAsVisibleRound() =
        tracerPendingDecision <- true
        tracerPendingDoubleDamage <- false

    member _.HasPending(now: TimeSpan) = pending && now <= pendingUntil
    member this.IsExpired(now: TimeSpan) = pending && now > pendingUntil
    member this.CanProbe(now: TimeSpan) = this.HasPending(now) && now >= nextProbeAt
    member _.DelayNextProbe(now: TimeSpan) = nextProbeAt <- now + probeInterval

    member _.CanSampleTracer(now: TimeSpan) =
        if now < nextTracerSampleAt then
            false
        else
            nextTracerSampleAt <- now + tracerSampleInterval
            true

    member _.Clear() =
        pending <- false
        tracerPendingDecision <- false
        tracerPendingDoubleDamage <- false

    member this.ClearAll() =
        this.Clear()
        tracerMagazines.Clear()
        nextTracerSampleAt <- TimeSpan.Zero

    member _.TryAcceptFmjFallbackImpact(weaponHash: uint32, target: Ped, impact: Vector3, now: TimeSpan) =
        if hasFmjFallbackImpact &&
           lastFmjFallbackWeaponHash = weaponHash &&
           lastFmjFallbackTargetHandle = target.Value &&
           now - lastFmjFallbackAt < duplicateInterval &&
           Vector3.DistanceSquared(lastFmjFallbackImpact, impact) <= duplicateDistanceSquared then
            false
        else
            hasFmjFallbackImpact <- true
            lastFmjFallbackWeaponHash <- weaponHash
            lastFmjFallbackTargetHandle <- target.Value
            lastFmjFallbackImpact <- impact
            lastFmjFallbackAt <- now
            true

    member _.TryAcceptIncendiaryCoverImpact(weaponHash: uint32, target: Ped, impact: Vector3, now: TimeSpan) =
        if hasIncendiaryCoverImpact &&
           lastIncendiaryCoverWeaponHash = weaponHash &&
           lastIncendiaryCoverTargetHandle = target.Value &&
           now - lastIncendiaryCoverAt < duplicateInterval &&
           Vector3.DistanceSquared(lastIncendiaryCoverImpact, impact) <= duplicateDistanceSquared then
            false
        else
            hasIncendiaryCoverImpact <- true
            lastIncendiaryCoverWeaponHash <- weaponHash
            lastIncendiaryCoverTargetHandle <- target.Value
            lastIncendiaryCoverImpact <- impact
            lastIncendiaryCoverAt <- now
            true

    member _.ShouldIgniteIncendiary(subsequentChancePercent: int) =
        if incendiaryFirstIgnitionAvailable then
            incendiaryFirstIgnitionAvailable <- false
            true
        elif subsequentChancePercent <= 0 then false
        else subsequentChancePercent >= 100 || probability.Next(100) < subsequentChancePercent

    member _.ShouldApplyCriticalShot(profile: CriticalShotProfile) =
        profile.IsEnabled &&
        (profile.ChancePercent >= 100 || probability.Next(100) < profile.ChancePercent)