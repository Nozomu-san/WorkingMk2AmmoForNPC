namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic
open System.Diagnostics
open System.Numerics
open StandardGameOperations.Source

[<Struct>]
type private CandidateBaseline =
    { Ped: Ped
      Position: Vector3
      PedVitals: PedVitals
      Vehicle: Vehicle
      VehicleVitals: VehicleVitals }
    member this.HasVehicle = this.Vehicle.Value <> 0

[<Struct>]
type private CandidateDamage =
    { Baseline: CandidateBaseline
      Ped: PedDamageSnapshot
      Vehicle: VehicleDamageSnapshot
      PedEvidence: bool
      VehicleEvidence: bool }

[<Sealed>]
type private PendingExplosion(
    position: Vector3,
    rule: AmmoRule,
    resolveAtHostFrame: uint64,
    candidates: CandidateBaseline array) =

    member _.Position = position
    member _.Rule = rule
    member _.ResolveAtHostFrame = resolveAtHostFrame
    member _.Candidates = candidates

[<Sealed>]
type private RegisteredShooter(shooter: Ped) =
    member _.Shooter = shooter
    member _.State = ShooterState()
    member val Rule : AmmoRule option = None with get, set
    member val PendingCandidates : CandidateBaseline array = [||] with get, set
    member val PendingCandidatesCaptured = false with get, set
    member val RecentImpact : struct (uint32 * Vector3 * TimeSpan) option = None with get, set
    member val NextLoadoutCheckAt = TimeSpan.Zero with get, set

[<Sealed>]
type internal Mk2Runtime(game: GameAccess) =
    do ArgumentNullException.ThrowIfNull(game)

    let pollInterval = TimeSpan.FromMilliseconds 33.0
    let discoveryInterval = TimeSpan.FromMilliseconds 250.0
    let loadoutCheckInterval = TimeSpan.FromMilliseconds 250.0
    let cadenceClock = Stopwatch.StartNew()
    let explosiveSettlementDelayFrames = 2UL
    let recentImpactLifetime = TimeSpan.FromMilliseconds 150.0
    let fmjCoverMaximumPerpendicularDistance = 1.5f
    let fmjCoverMinimumPathFraction = 0.02f
    let fmjCoverMaximumPathFraction = 1.05f
    let incendiaryCoverIgnitionRadiusSquared = 1.25f * 1.25f

    let effects = Mk2Effects(game)
    let shooters = Dictionary<int, RegisteredShooter>()
    let explosiveSettlements = ResizeArray<PendingExplosion>()
    let removalBuffer = ResizeArray<int>()
    let mutable nextPollAt = TimeSpan.Zero
    let mutable nextDiscoveryAt = TimeSpan.Zero
    let mutable lastObservedHostFrame = UInt64.MaxValue

    let clearPending (registration: RegisteredShooter) =
        registration.PendingCandidates <- [||]
        registration.PendingCandidatesCaptured <- false
        registration.RecentImpact <- None
        registration.State.Clear()

    let addCandidate
        (shooter: Ped)
        (seen: HashSet<int>)
        (candidates: ResizeArray<Ped>)
        (ped: Ped) =
        if ped.Value <> 0 &&
           ped.Value <> shooter.Value &&
           seen.Add(ped.Value) &&
           game.IsLivingHumanPed(ped) then
            candidates.Add(ped)

    let captureCandidates (shooter: Ped) =
        let seen = HashSet<int>()
        let peds = ResizeArray<Ped>()

        for ped in game.GetAllPeds() do
            addCandidate shooter seen peds ped

        peds
        |> Seq.map (fun ped ->
            let position = game.GetPosition(ped)
            let vehicle = game.GetCurrentVehicle(ped)
            if game.IsValidVehicle(vehicle) then
                { Ped = ped
                  Position = position
                  PedVitals = game.ReadPedVitals(ped)
                  Vehicle = vehicle
                  VehicleVitals = game.ReadVehicleVitals(vehicle) }
            else
                { Ped = ped
                  Position = position
                  PedVitals = game.ReadPedVitals(ped)
                  Vehicle = Unchecked.defaultof<Vehicle>
                  VehicleVitals = Unchecked.defaultof<VehicleVitals> })
        |> Seq.toArray

    let capturePendingCandidates
        (registration: RegisteredShooter) =
        registration.PendingCandidates <-
            captureCandidates registration.Shooter
        registration.PendingCandidatesCaptured <- true

    let rememberImpact
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (impact: Vector3)
        (now: TimeSpan) =
        registration.RecentImpact <-
            Some(struct (rule.WeaponHash, impact, now))

    let tryGetRecentImpact
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (now: TimeSpan) =
        match registration.RecentImpact with
        | Some struct (weaponHash, impact, observedAt)
            when weaponHash = rule.WeaponHash &&
                 now >= observedAt &&
                 now - observedAt <= recentImpactLifetime ->
            Some impact
        | _ ->
            None

    let impactOnTargetPath
        (shooter: Ped)
        (target: Ped)
        (impact: Vector3) =
        game.IsImpactOnTargetPath(
            shooter,
            target,
            impact,
            fmjCoverMaximumPerpendicularDistance,
            fmjCoverMinimumPathFraction,
            fmjCoverMaximumPathFraction)

    let tryFindImpactCandidate
        (registration: RegisteredShooter)
        (shooter: Ped)
        (impact: Vector3)
        (predicate: CandidateBaseline -> bool) =
        registration.PendingCandidates
        |> Array.filter (fun candidate ->
            game.IsLivingHumanPed(candidate.Ped) &&
            predicate candidate &&
            impactOnTargetPath shooter candidate.Ped impact)
        |> Array.sortBy (fun candidate ->
            Vector3.DistanceSquared(candidate.Position, impact))
        |> Array.tryHead

    let tryApplyFmjCoverFallback
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (now: TimeSpan) =
        let shooter = registration.Shooter
        match tryGetRecentImpact registration rule now with
        | Some impact ->
            match tryFindImpactCandidate
                      registration
                      shooter
                      impact
                      (fun _ -> true) with
            | Some candidate
                when registration.State.TryAcceptFmjFallbackImpact(
                         rule.WeaponHash,
                         candidate.Ped,
                         impact,
                         now) ->
                match game.TryGetWeaponDamage(rule.WeaponHash) with
                | Some nominalDamage ->
                    effects.ApplyTransferredFullMetalJacketDamage(
                        candidate.Ped,
                        nominalDamage)
                    |> ignore
                    true
                | None -> false
            | _ -> false
        | None -> false

    let tryApplyIncendiaryCoverFallback
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (now: TimeSpan) =
        let shooter = registration.Shooter
        match tryGetRecentImpact registration rule now with
        | Some impact ->
            let isEligible (candidate: CandidateBaseline) =
                game.IsPedInCover(candidate.Ped) &&
                Vector3.DistanceSquared(
                    candidate.Position,
                    impact) <= incendiaryCoverIgnitionRadiusSquared

            match tryFindImpactCandidate
                      registration
                      shooter
                      impact
                      isEligible with
            | Some candidate
                when registration.State.TryAcceptIncendiaryCoverImpact(
                         rule.WeaponHash,
                         candidate.Ped,
                         impact,
                         now) ->
                effects.TryApplyIncendiaryCoverIgnition(
                    candidate.Ped,
                    now,
                    registration.State)
                |> ignore
                true
            | _ -> false
        | None -> false

    let tryCaptureCandidateDamage
        (shooter: Ped)
        (weaponHash: uint32)
        (candidate: CandidateBaseline) =
        let pedEvidence =
            game.WasDamagedByWeapon(
                candidate.Ped,
                shooter,
                weaponHash)

        let vehicleEvidence =
            candidate.HasVehicle &&
            game.IsValidVehicle(candidate.Vehicle) &&
            game.WasDamagedByWeapon(
                candidate.Vehicle,
                shooter,
                weaponHash)

        if not pedEvidence && not vehicleEvidence then
            None
        else
            let pedDamage =
                game.CapturePedDamage(
                    candidate.Ped,
                    candidate.PedVitals)

            let vehicleDamage =
                if candidate.HasVehicle &&
                   game.IsValidVehicle(candidate.Vehicle) then
                    game.CaptureVehicleDamage(
                        candidate.Vehicle,
                        candidate.VehicleVitals)
                else
                    Unchecked.defaultof<VehicleDamageSnapshot>

            Some
                { Baseline = candidate
                  Ped = pedDamage
                  Vehicle = vehicleDamage
                  PedEvidence = pedEvidence
                  VehicleEvidence = vehicleEvidence }

    let tryFindDamagedCandidate
        (registration: RegisteredShooter)
        (rule: AmmoRule) =
        let shooter = registration.Shooter
        registration.PendingCandidates
        |> Array.tryPick (tryCaptureCandidateDamage shooter rule.WeaponHash)

    let clearDamageEvidence (damage: CandidateDamage) =
        game.ClearDamage(damage.Baseline.Ped)
        if damage.Baseline.HasVehicle &&
           game.IsValidVehicle(damage.Baseline.Vehicle) then
            game.ClearDamage(damage.Baseline.Vehicle)

    let applyDamageEffect
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (damage: CandidateDamage)
        (now: TimeSpan) =
        let ped = damage.Ped
        let vehicle = damage.Vehicle
        let state = registration.State
        let victim = damage.Baseline.Ped

        match rule.Effect with
        | Mk2AmmoEffect.HollowPoint ->
            if damage.PedEvidence && ped.HasDamage then
                effects.ApplyHollowPoint(ped) |> ignore
                effects.ApplyCriticalShot(
                    ped,
                    rule.CriticalShot,
                    state)
                |> ignore
                true
            else
                false

        | Mk2AmmoEffect.ArmorPiercing ->
            if damage.PedEvidence && ped.HasDamage then
                effects.ApplyArmorPiercing(ped) |> ignore
                effects.ApplyCriticalShot(
                    ped,
                    rule.CriticalShot,
                    state)
                |> ignore
                true
            elif damage.VehicleEvidence && vehicle.HasDamage then
                effects.ApplyTransferredArmorPiercingDamage(
                    victim,
                    vehicle.BodyHealthLoss)
                |> ignore
                true
            else
                false

        | Mk2AmmoEffect.Incendiary ->
            let mutable applied = false

            if damage.VehicleEvidence && vehicle.HasDamage then
                effects.ApplyIncendiaryToVehicle(vehicle) |> ignore
                applied <- true

            if damage.PedEvidence && ped.HasDamage then
                effects.ApplyIncendiary(
                    ped,
                    now,
                    state)
                |> ignore
                effects.ApplyCriticalShot(
                    ped,
                    rule.CriticalShot,
                    state)
                |> ignore
                applied <- true

            applied

        | Mk2AmmoEffect.FullMetalJacket ->
            let mutable applied = false

            if damage.VehicleEvidence && vehicle.HasDamage then
                let vehicleEffect =
                    effects.ApplyFullMetalJacket(vehicle)

                applied <- vehicleEffect.Applied

                if not ped.HasDamage then
                    let basis =
                        if vehicleEffect.TransferableDirectHealthLoss > 0.0f then
                            Some vehicleEffect.TransferableDirectHealthLoss
                        else
                            game.TryGetWeaponDamage(rule.WeaponHash)

                    match basis with
                    | Some value when value > 0.0f ->
                        effects.ApplyTransferredFullMetalJacketDamage(
                            victim,
                            value)
                        |> ignore
                        applied <- true
                    | _ -> ()

            if damage.PedEvidence && ped.HasDamage then
                effects.ApplyFullMetalJacketToPed(ped) |> ignore
                effects.ApplyCriticalShot(
                    ped,
                    rule.CriticalShot,
                    state)
                |> ignore
                applied <- true

            applied

        | Mk2AmmoEffect.Tracer ->
            let shouldDoubleDamage =
                state.CurrentTracerShouldDoubleDamage

            let mutable applied = false

            if damage.VehicleEvidence && vehicle.HasDamage then
                effects.ApplyTracerToVehicle(
                    vehicle,
                    shouldDoubleDamage)
                |> ignore
                applied <- true

            if damage.PedEvidence && ped.HasDamage then
                effects.ApplyTracerToPed(
                    ped,
                    shouldDoubleDamage)
                |> ignore
                effects.ApplyCriticalShot(
                    ped,
                    rule.CriticalShot,
                    state)
                |> ignore
                applied <- true

            applied

        | Mk2AmmoEffect.Explosive ->
            false

    let tryResolvePendingHit
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (now: TimeSpan) =
        if not registration.PendingCandidatesCaptured then
            false
        else
            match tryFindDamagedCandidate registration rule with
            | Some damage ->
                let applied =
                    applyDamageEffect registration rule damage now
                clearDamageEvidence damage
                applied

            | None ->
                match rule.Effect with
                | Mk2AmmoEffect.FullMetalJacket ->
                    tryApplyFmjCoverFallback registration rule now
                | Mk2AmmoEffect.Incendiary ->
                    tryApplyIncendiaryCoverFallback registration rule now
                | _ ->
                    false

    let observeTracer
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (now: TimeSpan) =
        let shooter = registration.Shooter
        let isReloading = game.IsReloading(shooter)

        match game.TryGetAmmoInClip(shooter, rule.WeaponHash) with
        | Some clipAmmo ->
            registration.State.ObserveTracerMagazine(
                rule.WeaponHash,
                RuleCatalog.tracerCadence rule.WeaponHash,
                clipAmmo,
                isReloading,
                now)
        | None when isReloading ->
            registration.State.ResetTracerMagazine(
                rule.WeaponHash,
                None)
        | None when
            game.IsShooting(shooter) &&
            registration.State.BeginShot(now) ->
            registration.State.MarkTracerFallbackAsVisibleRound()
        | _ -> ()

        if registration.State.HasPending(now) &&
           not registration.PendingCandidatesCaptured then
            capturePendingCandidates registration

    let captureExplosiveImpact
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (hostFrameIndex: uint64)
        (position: Vector3) =
        let shooter = registration.Shooter
        let explosionType = RuleCatalog.explosionType rule

        if explosionType >= 0 then
            let candidates = captureCandidates shooter

            explosiveSettlements.Add(
                PendingExplosion(
                    position,
                    rule,
                    hostFrameIndex + explosiveSettlementDelayFrames,
                    candidates))

            game.AddOwnedExplosion(
                shooter,
                position,
                explosionType)

    let processExplosiveSettlements (hostFrameIndex: uint64) =
        let mutable index = explosiveSettlements.Count - 1

        while index >= 0 do
            let settlement = explosiveSettlements[index]

            if hostFrameIndex >= settlement.ResolveAtHostFrame then
                for candidate in settlement.Candidates do
                    if game.IsValidPed(candidate.Ped) &&
                       not (game.IsDead(candidate.Ped)) then
                        let pedDamage =
                            game.CapturePedDamage(
                                candidate.Ped,
                                candidate.PedVitals)

                        if pedDamage.HasDamage then
                            effects.ApplyExplosiveTotalDamageMultiplier(
                                pedDamage,
                                candidate.HasVehicle)
                            |> ignore
                        else
                            match game.TryGetWeaponDamage(
                                      settlement.Rule.WeaponHash,
                                      settlement.Rule.ComponentHash) with
                            | Some nominalDamage ->
                                effects.ApplyExplosiveNominalDamage(
                                    candidate.Ped,
                                    nominalDamage,
                                    candidate.HasVehicle)
                                |> ignore
                            | None -> ()

                        game.TryRagdoll(candidate.Ped) |> ignore

                        game.ClearDamage(candidate.Ped)

                        if candidate.HasVehicle &&
                           game.IsValidVehicle(candidate.Vehicle) then
                            game.ClearDamage(candidate.Vehicle)

                explosiveSettlements.RemoveAt(index)

            index <- index - 1

    let activateRule
        (registration: RegisteredShooter)
        (rule: AmmoRule)
        (clockNow: TimeSpan) =
        match registration.Rule with
        | Some previous when previous = rule -> ()
        | _ ->
            registration.Rule <- Some rule
            clearPending registration
            registration.State.ClearAll()

            if rule.Effect.IsTracer then
                match game.TryGetAmmoInClip(
                          registration.Shooter,
                          rule.WeaponHash) with
                | Some clipAmmo ->
                    registration.State.PrimeTracerMagazine(
                        rule.WeaponHash,
                        clipAmmo,
                        game.IsReloading(registration.Shooter))
                | None -> ()

        registration.NextLoadoutCheckAt <-
            clockNow + loadoutCheckInterval

    let tryRefreshRule
        (registration: RegisteredShooter)
        (clockNow: TimeSpan) =
        match RuleCatalog.tryResolve game registration.Shooter with
        | Some rule ->
            activateRule registration rule clockNow
            true
        | None ->
            registration.Rule <- None
            clearPending registration
            registration.State.ClearAll()
            false

    let discoverShooters (clockNow: TimeSpan) =
        let player = game.PlayerPed()

        for ped in game.GetAllPeds() do
            if ped.Value <> player.Value &&
               not (shooters.ContainsKey(ped.Value)) &&
               game.IsLivingHumanPed(ped) then
                match RuleCatalog.tryResolve game ped with
                | Some rule ->
                    let registration =
                        RegisteredShooter(ped)
                    activateRule registration rule clockNow
                    shooters.Add(
                        ped.Value,
                        registration)
                | None -> ()

    let observeShooterFrame
        (registration: RegisteredShooter)
        (hostFrameIndex: uint64)
        (now: TimeSpan) =
        let shooter = registration.Shooter

        if game.IsLivingHumanPed(shooter) then
            match registration.Rule with
            | None -> ()
            | Some rule ->
                let selectedWeapon = game.GetSelectedWeapon(shooter)
                if selectedWeapon = rule.WeaponHash &&
                   game.HasWeaponComponent(
                       shooter,
                       rule.WeaponHash,
                       rule.ComponentHash) then
                    match game.TryGetLastWeaponImpactCoordinate(shooter) with
                    | Some impact ->
                        rememberImpact registration rule impact now
                        if rule.Effect = Mk2AmmoEffect.Explosive &&
                           game.IsShooting(shooter) &&
                           not (game.IsPerformingMeleeAction(shooter)) then
                            captureExplosiveImpact
                                registration
                                rule
                                hostFrameIndex
                                impact
                    | None -> ()

                    if rule.Effect <> Mk2AmmoEffect.Explosive &&
                       not rule.Effect.IsTracer &&
                       game.IsShooting(shooter) &&
                       registration.State.BeginShot(now) then
                        capturePendingCandidates registration

    let processShooter
        (registration: RegisteredShooter)
        (hostFrameIndex: uint64)
        (now: TimeSpan) =
        let shooter = registration.Shooter

        if not (game.IsLivingHumanPed(shooter)) then
            false
        else
            let clockNow = cadenceClock.Elapsed

            let loadoutValid =
                if Option.isNone registration.Rule ||
                   clockNow >= registration.NextLoadoutCheckAt then
                    tryRefreshRule registration clockNow
                else
                    true

            if not loadoutValid then
                false
            else
                let rule = registration.Rule.Value

                match rule.Effect with
                | Mk2AmmoEffect.Explosive ->
                    clearPending registration

                | _ ->
                    if rule.Effect.IsTracer &&
                       registration.State.CanSampleTracer(now) then
                        observeTracer registration rule now

                    if registration.State.HasPending(now) &&
                       registration.State.CanProbe(now) then
                        registration.State.DelayNextProbe(now)

                        if tryResolvePendingHit
                               registration
                               rule
                               now then
                            clearPending registration

                    if registration.State.IsExpired(now) then
                        clearPending registration

                true

    member _.IsSupported = game.IsSupported
    member _.ShooterCount = shooters.Count

    member _.Advance(
        hostFrameIndex: uint64,
        now: TimeSpan) =
        let clockNow = cadenceClock.Elapsed

        if clockNow >= nextDiscoveryAt then
            discoverShooters clockNow
            nextDiscoveryAt <-
                cadenceClock.Elapsed + discoveryInterval

        if hostFrameIndex <> lastObservedHostFrame then
            lastObservedHostFrame <- hostFrameIndex
            for registration in shooters.Values do
                observeShooterFrame
                    registration
                    hostFrameIndex
                    now

        if cadenceClock.Elapsed >= nextPollAt then
            removalBuffer.Clear()

            for pair in shooters do
                if not (
                    processShooter
                        pair.Value
                        hostFrameIndex
                        now) then
                    removalBuffer.Add(pair.Key)

            for handle in removalBuffer do
                match shooters.TryGetValue(handle) with
                | true, registration ->
                    registration.State.ClearAll()
                | _ -> ()

                shooters.Remove(handle) |> ignore

            processExplosiveSettlements hostFrameIndex
            effects.Advance(now)

            nextPollAt <-
                cadenceClock.Elapsed + pollInterval

    member _.Clear() =
        effects.StopAll()

        for registration in shooters.Values do
            registration.State.ClearAll()

        shooters.Clear()
        explosiveSettlements.Clear()
        removalBuffer.Clear()
        nextPollAt <- TimeSpan.Zero
        nextDiscoveryAt <- TimeSpan.Zero
        lastObservedHostFrame <- UInt64.MaxValue