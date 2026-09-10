namespace WorkingMk2AmmoForNPC.Source

open System
open System.Collections.Generic
open LowLevelEvents.Source
open LocalNativeMemories.Source
open StandardGameOperations.Source

[<Sealed>]
type internal Mk2Runtime(
    game: GameAccess,
    entityPools: ILocalEntityPools,
    configuration: Mk2EffectiveConfiguration,
    lowLevelEvents: ILowLevelEventStream,
    damageEvents: ILowLevelDamageStream,
    weaponEvents: ILowLevelWeaponEventStream) =
    do ArgumentNullException.ThrowIfNull(game)
    do ArgumentNullException.ThrowIfNull(entityPools)
    do ArgumentNullException.ThrowIfNull(lowLevelEvents)
    do ArgumentNullException.ThrowIfNull(damageEvents)
    do ArgumentNullException.ThrowIfNull(weaponEvents)

    let subscriptionCapacity = 2048
    let maximumEventsPerTick = 1024
    let shotLifetime = TimeSpan.FromMilliseconds 1250.0
    let shooterStateLifetime = TimeSpan.FromSeconds 30.0
    let shotgunFallbackWindow = TimeSpan.FromMilliseconds 150.0
    let coverMaximumPerpendicularDistance = 1.5f
    let coverMinimumPathFraction = 0.02f
    let coverMaximumPathFraction = 1.05f

    let damages = damageEvents.SubscribeDamage(subscriptionCapacity)
    let aimedAt = weaponEvents.SubscribeGunAimedAt(subscriptionCapacity)
    let gunShots = weaponEvents.SubscribeGunShots(subscriptionCapacity)
    let impacts = weaponEvents.SubscribeBulletImpacts(subscriptionCapacity)
    let effects = Mk2Effects(game)
    let shooterStates = Dictionary<int, ShooterRuntimeState>()
    let aimContexts = Dictionary<int, AimContext>()
    let victimStates = Dictionary<int, VictimRuntimeState>()
    let shots = Dictionary<struct (uint64 * uint32), ResizeArray<ShotContext>>()
    let resolvedRuleCache = Dictionary<struct (int * uint32), ComponentRule>()
    let shotgunFallback = Dictionary<struct (int * int * uint32), TimeSpan>()
    let shotKeysToRemove = ResizeArray<struct (uint64 * uint32)>()
    let shooterHandlesToRemove = ResizeArray<int>()
    let aimShooterHandlesToRemove = ResizeArray<int>()
    let victimHandlesToRemove = ResizeArray<int>()
    let resolvedRuleKeysToRemove = ResizeArray<struct (int * uint32)>()
    let shotgunKeysToRemove = ResizeArray<struct (int * int * uint32)>()

    let mutable damageHead : EntityDamageEvent option = None
    let mutable aimHead : GunAimedAtEvent option = None
    let mutable gunShotHead : GunShotEvent option = None
    let mutable impactHead : BulletImpactEvent option = None
    let mutable damageContinuityRevision = damageEvents.ContinuityRevision
    let mutable weaponContinuityRevision = weaponEvents.ContinuityRevision
    let mutable damageDropped = damages.DroppedCount
    let mutable aimDropped = aimedAt.DroppedCount
    let mutable gunShotDropped = gunShots.DroppedCount
    let mutable impactDropped = impacts.DroppedCount
    let mutable disposed = false
    let mutable aimReceivedCount = 0UL
    let mutable aimAcceptedCount = 0UL
    let mutable aimRejectedCount = 0UL
    let mutable victimLastKnownCreatedCount = 0UL
    let mutable victimLastKnownReusedCount = 0UL
    let mutable victimLastKnownRefreshedCount = 0UL
    let mutable pedPopulationRevision = UInt64.MaxValue
    let mutable pedPopulationSeededCount = 0UL
    let mutable victimDeltaObservedCount = 0UL
    let mutable damageReceivedCount = 0UL
    let mutable victimRejectedCount = 0UL
    let mutable shooterAcceptedCount = 0UL
    let mutable shooterRejectedCount = 0UL
    let mutable ruleMatchedCount = 0UL
    let mutable ruleNotMatchedCount = 0UL
    let mutable ruleBucketFoundOnMissCount = 0UL
    let mutable ruleBucketMissingOnMissCount = 0UL
    let mutable componentPresentButInactiveOnMissCount = 0UL
    let mutable componentMissingOnMissCount = 0UL
    let mutable componentActiveAfterMissCount = 0UL
    let mutable shotMatchedCount = 0UL
    let mutable shotNotMatchedCount = 0UL
    let mutable magnitudeAvailableCount = 0UL
    let mutable magnitudeUnavailableCount = 0UL
    let mutable settlementAttemptedCount = 0UL
    let mutable settlementAppliedCount = 0UL
    let mutable settlementNoChangeCount = 0UL
    let mutable lastStage = "Initialized"
    let mutable lastCulpritHandle = 0
    let mutable lastVictimHandle = 0
    let mutable lastWeaponHash = 0u
    let mutable lastComponentHash = 0u
    let mutable lastComponentName = String.Empty
    let mutable lastEffect = String.Empty
    let mutable lastCandidateComponentHash = 0u
    let mutable lastCandidateComponentName = String.Empty
    let mutable lastCandidatePresent = false
    let mutable lastCandidateActive = false
    let mutable lastCandidateSummary = String.Empty
    let mutable lastBeforeHealth = 0
    let mutable lastBeforeArmour = 0
    let mutable lastAfterHealth = 0
    let mutable lastAfterArmour = 0
    let mutable lastObservedDamage = 0
    let mutable lastBaseDamage = 0f
    let mutable lastMagnitudeSource = DamageMagnitudeSource.Unavailable

    let observeDamage (event: EntityDamageEvent) stage =
        lastStage <- stage
        lastCulpritHandle <- event.CulpritHandle
        lastVictimHandle <- event.VictimHandle
        lastWeaponHash <- event.WeaponHash
        lastComponentHash <- 0u
        lastComponentName <- String.Empty
        lastEffect <- String.Empty
        lastCandidateComponentHash <- 0u
        lastCandidateComponentName <- String.Empty
        lastCandidatePresent <- false
        lastCandidateActive <- false
        lastCandidateSummary <- String.Empty
        lastBaseDamage <- event.BaseDamage
        lastMagnitudeSource <- event.MagnitudeSource

    let observeRule (rule: ComponentRule) =
        lastComponentHash <- rule.ComponentHash
        lastComponentName <- rule.ComponentName
        lastEffect <- rule.Effect.ToString()

    let observeStage stage =
        lastStage <- stage

    let diagnoseRuleMiss (shooter: Ped) weaponHash =
        match RuleCatalog.tryGetCandidates weaponHash with
        | None ->
            ruleBucketMissingOnMissCount <- ruleBucketMissingOnMissCount + 1UL
            lastCandidateSummary <- "BucketMissing"
        | Some candidates ->
            ruleBucketFoundOnMissCount <- ruleBucketFoundOnMissCount + 1UL
            let states = ResizeArray<string>(candidates.Length)
            let mutable firstWitness : (ComponentRule * bool * bool) option = None
            let mutable presentWitness : (ComponentRule * bool * bool) option = None
            let mutable activeWitness : (ComponentRule * bool * bool) option = None
            for candidate in candidates do
                let present =
                    game.HasComponent(shooter, weaponHash, candidate.ComponentHash)
                let active =
                    game.IsComponentActive(shooter, weaponHash, candidate.ComponentHash)
                let presentText = if present then "1" else "0"
                let activeText = if active then "1" else "0"
                states.Add(
                    $"{candidate.ComponentName}:P{presentText}A{activeText}")
                if Option.isNone firstWitness then
                    firstWitness <- Some (candidate, present, active)
                if present && Option.isNone presentWitness then
                    presentWitness <- Some (candidate, present, active)
                if active && Option.isNone activeWitness then
                    activeWitness <- Some (candidate, present, active)
            lastCandidateSummary <- String.Join(",", states)
            let witness =
                match activeWitness with
                | Some value -> Some value
                | None ->
                    match presentWitness with
                    | Some value -> Some value
                    | None -> firstWitness
            match witness with
            | Some (candidate, present, active) ->
                lastCandidateComponentHash <- candidate.ComponentHash
                lastCandidateComponentName <- candidate.ComponentName
                lastCandidatePresent <- present
                lastCandidateActive <- active
            | None -> ()
            match activeWitness, presentWitness with
            | Some _, _ ->
                componentActiveAfterMissCount <- componentActiveAfterMissCount + 1UL
            | None, Some _ ->
                componentPresentButInactiveOnMissCount <-
                    componentPresentButInactiveOnMissCount + 1UL
            | None, None ->
                componentMissingOnMissCount <- componentMissingOnMissCount + 1UL

    let isPumpShotgunRule (rule: ComponentRule) =
        rule.ComponentName.StartsWith(
            "COMPONENT_PUMPSHOTGUN_MK2_",
            StringComparison.Ordinal)

    let discardBufferedInput () =
        damageHead <- None
        aimHead <- None
        gunShotHead <- None
        impactHead <- None
        let mutable damage = Unchecked.defaultof<EntityDamageEvent>
        while damages.TryRead(&damage) do ()
        let mutable aim = Unchecked.defaultof<GunAimedAtEvent>
        while aimedAt.TryRead(&aim) do ()
        let mutable gunShot = Unchecked.defaultof<GunShotEvent>
        while gunShots.TryRead(&gunShot) do ()
        let mutable impact = Unchecked.defaultof<BulletImpactEvent>
        while impacts.TryRead(&impact) do ()
        shots.Clear()
        aimContexts.Clear()
        resolvedRuleCache.Clear()
        shotgunFallback.Clear()

    let resetAfterContinuityBreak () =
        discardBufferedInput()
        damageContinuityRevision <- damageEvents.ContinuityRevision
        weaponContinuityRevision <- weaponEvents.ContinuityRevision
        damageDropped <- damages.DroppedCount
        aimDropped <- aimedAt.DroppedCount
        gunShotDropped <- gunShots.DroppedCount
        impactDropped <- impacts.DroppedCount

    let continuityBroken () =
        damageEvents.ContinuityRevision <> damageContinuityRevision ||
        weaponEvents.ContinuityRevision <> weaponContinuityRevision ||
        damages.DroppedCount <> damageDropped ||
        aimedAt.DroppedCount <> aimDropped ||
        gunShots.DroppedCount <> gunShotDropped ||
        impacts.DroppedCount <> impactDropped

    let tryGetShooterState (ped: Ped) now =
        match shooterStates.TryGetValue(ped.Value) with
        | true, state ->
            state.LastSeenAt <- now
            Some state
        | _ ->
            if not (game.IsNpcShooter(ped)) then
                None
            else
                let state = ShooterRuntimeState(ped, configuration.Balance, now)
                shooterStates.Add(ped.Value, state)
                Some state

    let tryResolveRule (shooter: Ped) weaponHash =
        let key = struct (shooter.Value, weaponHash)
        match resolvedRuleCache.TryGetValue(key) with
        | true, rule when game.HasComponent(shooter, weaponHash, rule.ComponentHash) ->
            Some rule
        | true, _ ->
            resolvedRuleCache.Remove(key) |> ignore
            match
                RuleCatalog.tryResolveComponent
                    weaponHash
                    (fun componentHash ->
                        game.HasComponent(shooter, weaponHash, componentHash))
            with
            | Some rule ->
                resolvedRuleCache[key] <- rule
                Some rule
            | None -> None
        | _ ->
            match
                RuleCatalog.tryResolveComponent
                    weaponHash
                    (fun componentHash ->
                        game.HasComponent(shooter, weaponHash, componentHash))
            with
            | Some rule ->
                resolvedRuleCache[key] <- rule
                Some rule
            | None -> None

    let createVictimLastKnown victimAddress (victim: Ped) sequence =
        let snapshot = game.ReadPedVitals(victim)
        let state = VictimRuntimeState(victimAddress, victim, snapshot, sequence)
        victimStates[victim.Value] <- state
        victimLastKnownCreatedCount <- victimLastKnownCreatedCount + 1UL
        state

    let ensureVictimLastKnown victimAddress (victim: Ped) sequence =
        match victimStates.TryGetValue(victim.Value) with
        | true, state when
            state.VictimAddress = 0UL ||
            victimAddress = 0UL ||
            state.VictimAddress = victimAddress ->
            if state.VictimAddress = 0UL && victimAddress <> 0UL then
                state.VictimAddress <- victimAddress
            let snapshot = game.ReadPedVitals(victim)
            let lastKnown = state.LastKnown
            let advanced =
                snapshot.Health >= lastKnown.Health &&
                snapshot.Armour >= lastKnown.Armour &&
                (snapshot.Health > lastKnown.Health ||
                 snapshot.Armour > lastKnown.Armour)
            if advanced then
                state.LastKnown <- snapshot
                state.Sequence <- sequence
                state.Revision <- state.Revision + 1UL
                victimLastKnownRefreshedCount <-
                    victimLastKnownRefreshedCount + 1UL
            else
                victimLastKnownReusedCount <- victimLastKnownReusedCount + 1UL
            state
        | true, _ ->
            createVictimLastKnown victimAddress victim sequence
        | _ ->
            createVictimLastKnown victimAddress victim sequence

    let synchronizePedPopulation () =
        let captured = entityPools.Capture(LocalPoolKinds.Peds)
        if captured.IsSuccess then
            let snapshot = captured.Value.Peds
            if snapshot.Revision <> pedPopulationRevision then
                let handles = snapshot.Handles.ToArray()
                for handle in handles do
                    if handle <> 0 && not (victimStates.ContainsKey(handle)) then
                        let ped = Ped(handle)
                        if game.IsValidPed(ped) then
                            createVictimLastKnown 0UL ped snapshot.Revision
                            |> ignore
                            pedPopulationSeededCount <-
                                pedPopulationSeededCount + 1UL
                pedPopulationRevision <- snapshot.Revision

    let tryResolveEngagement (shooter: Ped) =
        match aimContexts.TryGetValue(shooter.Value) with
        | true, aim when game.IsValidPed(aim.Victim) ->
            Some (CombatEngagement(
                shooter,
                aim.Victim,
                game.GetCurrentVehicle(aim.Victim)))
        | _ -> None

    let processAim (event: GunAimedAtEvent) now =
        aimReceivedCount <- aimReceivedCount + 1UL
        let shooter = Ped(event.ShooterHandle)
        let victim = Ped(event.VictimHandle)
        if not (game.IsNpcShooter(shooter)) ||
           not (game.IsValidPed(victim)) ||
           not (game.IsHuman(victim)) then
            aimRejectedCount <- aimRejectedCount + 1UL
            observeStage "AimRejected"
        else
            match tryGetShooterState shooter now with
            | None ->
                aimRejectedCount <- aimRejectedCount + 1UL
                observeStage "AimRejected"
            | Some _ ->
                match aimContexts.TryGetValue(shooter.Value) with
                | true, current when
                    current.VictimAddress = event.VictimAddress &&
                    current.Victim.Value = victim.Value ->
                    ()
                | _ ->
                    ensureVictimLastKnown event.VictimAddress victim event.Sequence
                    |> ignore
                    aimContexts[shooter.Value] <-
                        AimContext(
                            event.Sequence,
                            event.ShooterAddress,
                            shooter,
                            event.VictimAddress,
                            victim)
                    aimAcceptedCount <- aimAcceptedCount + 1UL
                    lastCulpritHandle <- shooter.Value
                    lastVictimHandle <- victim.Value
                    observeStage "AimAccepted"

    let addShot (shot: ShotContext) =
        let key = struct (shot.ShooterAddress, shot.WeaponHash)
        let bucket =
            match shots.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = ResizeArray<ShotContext>()
                shots.Add(key, value)
                value
        bucket.Add(shot)

    let tryFindMostRecentShot shooterAddress weaponHash sequence now =
        let key = struct (shooterAddress, weaponHash)
        match shots.TryGetValue(key) with
        | false, _ -> None
        | true, bucket ->
            let mutable index = bucket.Count - 1
            let mutable found : ShotContext option = None
            while index >= 0 && Option.isNone found do
                let candidate = bucket[index]
                if candidate.Sequence <= sequence && now <= candidate.ExpiresAt then
                    found <- Some candidate
                index <- index - 1
            found

    let impactOnTargetPath (shot: ShotContext) (engagement: CombatEngagement) impact =
        game.IsImpactOnTargetPath(
            shot.Shooter,
            engagement.Victim,
            impact,
            coverMaximumPerpendicularDistance,
            coverMinimumPathFraction,
            coverMaximumPathFraction)

    let applyExplosionAt (shot: ShotContext) position =
        if configuration.Balance.ExplosiveEnabled &&
           shot.Rule.ExplosionType >= 0 then
            game.AddOwnedExplosion(
                shot.Shooter,
                position,
                shot.Rule.ExplosionType)

    let resolveExplosionPosition (shot: ShotContext) victim =
        match shot.ImpactPosition with
        | Some position -> position
        | None ->
            match game.TryGetLastImpact(shot.Shooter) with
            | Some position -> position
            | None -> game.GetPosition(victim)

    let tryApplyImpactSecondaryEffect (shot: ShotContext) impact now =
        match shot.Rule.Effect, shot.Engagement with
        | Mk2AmmoEffect.Explosive, _ ->
            applyExplosionAt shot impact
        | Mk2AmmoEffect.Incendiary, Some engagement
            when configuration.RidiculousMode &&
                 configuration.Balance.IncendiaryDamageHandlerEnabled &&
                 not engagement.HasVehicle &&
                 game.IsValidPed(engagement.Victim) &&
                 game.IsPedInCover(engagement.Victim) &&
                 impactOnTargetPath shot engagement impact ->
            match shooterStates.TryGetValue(shot.Shooter.Value) with
            | true, shooterState ->
                effects.TryApplyIncendiaryCoverIgnition(
                    engagement.Victim,
                    now,
                    shooterState.Effects)
                |> ignore
            | _ -> ()
        | _ -> ()

    let processGunShot (event: GunShotEvent) now =
        let shooter = Ped(event.ShooterHandle)
        match tryGetShooterState shooter now with
        | None -> ()
        | Some shooterState ->
            match tryResolveRule shooter event.WeaponHash with
            | None -> ()
            | Some rule ->
                let tracerBoost =
                    if rule.Effect = Mk2AmmoEffect.Tracer &&
                       configuration.RidiculousMode &&
                       configuration.Balance.TracerDamageHandlerEnabled then
                        shooterState.Effects.ResolveTracerBoost(
                            event.WeaponHash,
                            rule.TracerCadence,
                            game.TryGetAmmoInClip(shooter, event.WeaponHash),
                            game.GetMaxAmmoInClip(shooter, event.WeaponHash),
                            game.IsReloading(shooter))
                    else
                        false
                addShot(
                    ShotContext(
                        event.Sequence,
                        event.ShooterAddress,
                        shooter,
                        event.WeaponHash,
                        rule,
                        tracerBoost,
                        now + shotLifetime,
                        tryResolveEngagement shooter))

    let processImpact (event: BulletImpactEvent) now =
        let shooter = Ped(event.ShooterHandle)
        match tryGetShooterState shooter now with
        | None -> ()
        | Some _ ->
            match
                tryFindMostRecentShot
                    event.ShooterAddress
                    event.WeaponHash
                    event.Sequence
                    now
            with
            | Some shot when not shot.ImpactApplied ->
                shot.ImpactApplied <- true
                shot.ImpactPosition <- Some event.ImpactPosition
                tryApplyImpactSecondaryEffect shot event.ImpactPosition now
            | _ -> ()

    let shouldSuppressStandaloneShotgunDamage
        (rule: ComponentRule)
        shooterHandle
        victimHandle
        weaponHash
        now =
        if not (isPumpShotgunRule rule) then
            false
        else
            let key = struct (shooterHandle, victimHandle, weaponHash)
            match shotgunFallback.TryGetValue(key) with
            | true, previous when now - previous <= shotgunFallbackWindow -> true
            | _ ->
                shotgunFallback[key] <- now
                false

    let commitVictimState
        (state: VictimRuntimeState)
        vitals
        sequence =
        state.LastKnown <- vitals
        state.Sequence <- sequence
        state.Revision <- state.Revision + 1UL

    let observeVictimDelta
        (event: EntityDamageEvent)
        (victim: Ped) =
        let after = game.ReadPedVitals(victim)
        match victimStates.TryGetValue(victim.Value) with
        | true, state when
            state.VictimAddress = 0UL ||
            state.VictimAddress = event.VictimAddress ->
            if state.VictimAddress = 0UL then
                state.VictimAddress <- event.VictimAddress
            let before = state.LastKnown
            let armourLoss = max 0 (before.Armour - after.Armour)
            let healthLoss = max 0 (before.Health - after.Health)
            let observedDamage = armourLoss + healthLoss
            let observation =
                {
                    Before = before
                    After = after
                    ArmourLoss = armourLoss
                    HealthLoss = healthLoss
                    ObservedDamage = observedDamage
                }
            lastBeforeHealth <- before.Health
            lastBeforeArmour <- before.Armour
            lastAfterHealth <- after.Health
            lastAfterArmour <- after.Armour
            lastObservedDamage <- observedDamage
            if observedDamage > 0 then
                victimDeltaObservedCount <- victimDeltaObservedCount + 1UL
            Some (state, observation)
        | true, _ ->
            let state = VictimRuntimeState(
                event.VictimAddress,
                victim,
                after,
                event.Sequence)
            victimStates[victim.Value] <- state
            victimLastKnownCreatedCount <- victimLastKnownCreatedCount + 1UL
            lastBeforeHealth <- after.Health
            lastBeforeArmour <- after.Armour
            lastAfterHealth <- after.Health
            lastAfterArmour <- after.Armour
            lastObservedDamage <- 0
            None
        | _ ->
            let state = VictimRuntimeState(
                event.VictimAddress,
                victim,
                after,
                event.Sequence)
            victimStates.Add(victim.Value, state)
            victimLastKnownCreatedCount <- victimLastKnownCreatedCount + 1UL
            lastBeforeHealth <- after.Health
            lastBeforeArmour <- after.Armour
            lastAfterHealth <- after.Health
            lastAfterArmour <- after.Armour
            lastObservedDamage <- 0
            None

    let canSettleDamage (victim: Ped) (observation: PedDamageObservation) =
        game.IsDead(victim) || observation.ObservedDamage > 0

    let applyDamageSettlement
        (rule: ComponentRule)
        tracerBoost
        (victim: Ped)
        (shooterState: ShooterRuntimeState)
        (observation: PedDamageObservation)
        now =
        if game.IsDead(victim) then
            game.SetPedArmour(victim, 0)
            game.SetPedHealth(victim, 0)
            true
        elif observation.ObservedDamage > 0 then
            effects.ApplyDirectPedDamage(
                rule,
                configuration.Balance,
                configuration.RidiculousMode,
                tracerBoost,
                victim,
                observation,
                now,
                shooterState.Effects)
        else
            false

    let applyObservedSettlement
        rule
        tracerBoost
        victim
        shooterState
        observation
        now =
        settlementAttemptedCount <- settlementAttemptedCount + 1UL
        observeStage "SettlementAttempted"
        if applyDamageSettlement
            rule
            tracerBoost
            victim
            shooterState
            observation
            now then
            settlementAppliedCount <- settlementAppliedCount + 1UL
            observeStage "SettlementApplied"
            true
        else
            settlementNoChangeCount <- settlementNoChangeCount + 1UL
            observeStage "SettlementNoChange"
            false

    let processDamage (event: EntityDamageEvent) now =
        damageReceivedCount <- damageReceivedCount + 1UL
        observeDamage event "DamageReceived"
        if event.MagnitudeSource = DamageMagnitudeSource.DamageProcess &&
           Single.IsFinite(event.BaseDamage) &&
           event.BaseDamage > 0f then
            magnitudeAvailableCount <- magnitudeAvailableCount + 1UL
        else
            magnitudeUnavailableCount <- magnitudeUnavailableCount + 1UL

        let shooter = Ped(event.CulpritHandle)
        let victim = Ped(event.VictimHandle)
        if not (game.IsValidPed(victim)) then
            victimRejectedCount <- victimRejectedCount + 1UL
            observeStage "VictimRejected"
        else
            let observation = observeVictimDelta event victim
            match observation with
            | None ->
                observeStage "LastKnownEstablishedAfterDamage"
            | Some (victimState, observation) ->
                let mutable committedVitals = observation.After
                match tryGetShooterState shooter now with
                | None ->
                    shooterRejectedCount <- shooterRejectedCount + 1UL
                    observeStage "ShooterRejected"
                | Some shooterState ->
                    shooterAcceptedCount <- shooterAcceptedCount + 1UL
                    observeStage "ShooterAccepted"
                    match tryResolveRule shooter event.WeaponHash with
                    | None ->
                        ruleNotMatchedCount <- ruleNotMatchedCount + 1UL
                        diagnoseRuleMiss shooter event.WeaponHash
                        observeStage "RuleNotMatched"
                    | Some resolvedRule ->
                        ruleMatchedCount <- ruleMatchedCount + 1UL
                        observeRule resolvedRule
                        observeStage "RuleMatched"
                        match
                            tryFindMostRecentShot
                                event.CulpritAddress
                                event.WeaponHash
                                event.Sequence
                                now
                        with
                        | Some shot ->
                            shotMatchedCount <- shotMatchedCount + 1UL
                            observeRule shot.Rule
                            observeStage "ShotMatched"
                            if shot.Rule.Effect = Mk2AmmoEffect.Explosive &&
                               not shot.ImpactApplied then
                                let position = resolveExplosionPosition shot victim
                                shot.ImpactApplied <- true
                                shot.ImpactPosition <- Some position
                                applyExplosionAt shot position
                            if not shot.DamageApplied &&
                               canSettleDamage victim observation then
                                shot.DamageApplied <- true
                                if applyObservedSettlement
                                    shot.Rule
                                    shot.TracerBoost
                                    victim
                                    shooterState
                                    observation
                                    now then
                                    committedVitals <- game.ReadPedVitals(victim)
                        | None ->
                            shotNotMatchedCount <- shotNotMatchedCount + 1UL
                            observeStage "ShotNotMatched"
                            if canSettleDamage victim observation &&
                               not (
                                   shouldSuppressStandaloneShotgunDamage
                                       resolvedRule
                                       shooter.Value
                                       victim.Value
                                       event.WeaponHash
                                       now) then
                                if resolvedRule.Effect = Mk2AmmoEffect.Explosive &&
                                   configuration.Balance.ExplosiveEnabled &&
                                   resolvedRule.ExplosionType >= 0 then
                                    let position =
                                        match game.TryGetLastImpact(shooter) with
                                        | Some impact -> impact
                                        | None -> game.GetPosition(victim)
                                    game.AddOwnedExplosion(
                                        shooter,
                                        position,
                                        resolvedRule.ExplosionType)
                                if applyObservedSettlement
                                    resolvedRule
                                    false
                                    victim
                                    shooterState
                                    observation
                                    now then
                                    committedVitals <- game.ReadPedVitals(victim)
                commitVictimState victimState committedVitals event.Sequence

    let fillHeads () =
        if Option.isNone damageHead then
            let mutable value = Unchecked.defaultof<EntityDamageEvent>
            if damages.TryRead(&value) then damageHead <- Some value
        if Option.isNone aimHead then
            let mutable value = Unchecked.defaultof<GunAimedAtEvent>
            if aimedAt.TryRead(&value) then aimHead <- Some value
        if Option.isNone gunShotHead then
            let mutable value = Unchecked.defaultof<GunShotEvent>
            if gunShots.TryRead(&value) then gunShotHead <- Some value
        if Option.isNone impactHead then
            let mutable value = Unchecked.defaultof<BulletImpactEvent>
            if impacts.TryRead(&value) then impactHead <- Some value

    let trySelectNextKind () =
        fillHeads()
        let mutable kind = -1
        let mutable sequence = UInt64.MaxValue
        match aimHead with
        | Some value when value.Sequence < sequence ->
            kind <- 0
            sequence <- value.Sequence
        | _ -> ()
        match gunShotHead with
        | Some value when value.Sequence < sequence ->
            kind <- 1
            sequence <- value.Sequence
        | _ -> ()
        match impactHead with
        | Some value when value.Sequence < sequence ->
            kind <- 2
            sequence <- value.Sequence
        | _ -> ()
        match damageHead with
        | Some value when value.Sequence < sequence ->
            kind <- 3
        | _ -> ()
        if kind < 0 then None else Some kind

    let processBufferedEvents now =
        let mutable remaining = maximumEventsPerTick
        let mutable continueLoop = true
        while remaining > 0 && continueLoop do
            match trySelectNextKind() with
            | None -> continueLoop <- false
            | Some 0 ->
                match aimHead with
                | Some value ->
                    aimHead <- None
                    processAim value now
                | None -> ()
                remaining <- remaining - 1
            | Some 1 ->
                match gunShotHead with
                | Some value ->
                    gunShotHead <- None
                    processGunShot value now
                | None -> ()
                remaining <- remaining - 1
            | Some 2 ->
                match impactHead with
                | Some value ->
                    impactHead <- None
                    processImpact value now
                | None -> ()
                remaining <- remaining - 1
            | Some 3 ->
                match damageHead with
                | Some value ->
                    damageHead <- None
                    processDamage value now
                | None -> ()
                remaining <- remaining - 1
            | Some _ -> continueLoop <- false

    let pruneShots now =
        shotKeysToRemove.Clear()
        for pair in shots do
            let bucket = pair.Value
            let mutable index = bucket.Count - 1
            while index >= 0 do
                let shot = bucket[index]
                if now > shot.ExpiresAt ||
                   (shot.DamageApplied && shot.ImpactApplied) then
                    bucket.RemoveAt(index)
                index <- index - 1
            if bucket.Count = 0 then shotKeysToRemove.Add(pair.Key)
        for key in shotKeysToRemove do shots.Remove(key) |> ignore

    let pruneShooters now =
        shooterHandlesToRemove.Clear()
        for pair in shooterStates do
            if now - pair.Value.LastSeenAt >= shooterStateLifetime then
                shooterHandlesToRemove.Add(pair.Key)
        for handle in shooterHandlesToRemove do
            match shooterStates.TryGetValue(handle) with
            | true, state -> state.Effects.Clear()
            | _ -> ()
            shooterStates.Remove(handle) |> ignore
            resolvedRuleKeysToRemove.Clear()
            for pair in resolvedRuleCache do
                let struct (shooterHandle, _) = pair.Key
                if shooterHandle = handle then
                    resolvedRuleKeysToRemove.Add(pair.Key)
            for key in resolvedRuleKeysToRemove do
                resolvedRuleCache.Remove(key) |> ignore

    let pruneAimContexts () =
        aimShooterHandlesToRemove.Clear()
        for pair in aimContexts do
            if not (game.IsValidPed(pair.Value.Shooter)) ||
               not (game.IsValidPed(pair.Value.Victim)) then
                aimShooterHandlesToRemove.Add(pair.Key)
        for handle in aimShooterHandlesToRemove do
            aimContexts.Remove(handle) |> ignore

    let pruneVictims () =
        victimHandlesToRemove.Clear()
        for pair in victimStates do
            if not (game.IsValidPed(pair.Value.Victim)) then
                victimHandlesToRemove.Add(pair.Key)
        for handle in victimHandlesToRemove do
            victimStates.Remove(handle) |> ignore

    let pruneShotgunFallback now =
        shotgunKeysToRemove.Clear()
        for pair in shotgunFallback do
            if now - pair.Value > TimeSpan.FromSeconds 1.0 then
                shotgunKeysToRemove.Add(pair.Key)
        for key in shotgunKeysToRemove do
            shotgunFallback.Remove(key) |> ignore

    member _.CreateDiagnosticPayload() =
        let low = lowLevelEvents.Diagnostics
        let weaponHashText = lastWeaponHash.ToString("X8")
        let componentHashText = lastComponentHash.ToString("X8")
        let candidateComponentHashText = lastCandidateComponentHash.ToString("X8")
        String.Join(
            ";",
            [|
                $"LLE.DamageRaw={low.DamageRaw}"
                $"LLE.DamagePublished={low.DamagePublished}"
                $"LLE.DamageDropped={low.DamageDropped}"
                $"LLE.DamageIdentityQueued={low.DamageIdentityQueued}"
                $"LLE.DamageVictimIdentityResolved={low.DamageVictimIdentityResolved}"
                $"LLE.DamageCulpritIdentityResolved={low.DamageCulpritIdentityResolved}"
                $"LLE.DamageVictimIdentityExpired={low.DamageVictimIdentityExpired}"
                $"LLE.DamageCulpritIdentityExpired={low.DamageCulpritIdentityExpired}"
                $"LLE.DamageIdentityPending={low.DamageIdentityPending}"
                $"LLE.DamageMagnitudeAvailable={low.DamageMagnitudeAvailable}"
                $"LLE.DamageMagnitudeUnavailable={low.DamageMagnitudeUnavailable}"
                $"LLE.GunAimedAtRaw={low.GunAimedAtRaw}"
                $"LLE.GunAimedAtReaction={low.GunAimedAtReaction}"
                $"LLE.GunAimedAtGroup={low.GunAimedAtGroup}"
                $"LLE.GunAimedAtGlobal={low.GunAimedAtGlobal}"
                $"LLE.GunAimedAtPublished={low.GunAimedAtPublished}"
                $"LLE.GunAimedAtMissingSource={low.GunAimedAtMissingSource}"
                $"LLE.GunAimedAtMissingTarget={low.GunAimedAtMissingTarget}"
                $"Mk2.AimReceived={aimReceivedCount}"
                $"Mk2.AimAccepted={aimAcceptedCount}"
                $"Mk2.AimRejected={aimRejectedCount}"
                $"Mk2.AimContexts={aimContexts.Count}"
                $"Mk2.VictimStates={victimStates.Count}"
                $"Mk2.PedPopulationRevision={pedPopulationRevision}"
                $"Mk2.PedPopulationSeeded={pedPopulationSeededCount}"
                $"Mk2.VictimLastKnownCreated={victimLastKnownCreatedCount}"
                $"Mk2.VictimLastKnownReused={victimLastKnownReusedCount}"
                $"Mk2.VictimLastKnownRefreshed={victimLastKnownRefreshedCount}"
                $"Mk2.VictimDeltaObserved={victimDeltaObservedCount}"
                $"Mk2.DamageReceived={damageReceivedCount}"
                $"Mk2.VictimRejected={victimRejectedCount}"
                $"Mk2.ShooterAccepted={shooterAcceptedCount}"
                $"Mk2.ShooterRejected={shooterRejectedCount}"
                $"Mk2.RuleMatched={ruleMatchedCount}"
                $"Mk2.RuleNotMatched={ruleNotMatchedCount}"
                $"Mk2.RuleBucketFoundOnMiss={ruleBucketFoundOnMissCount}"
                $"Mk2.RuleBucketMissingOnMiss={ruleBucketMissingOnMissCount}"
                $"Mk2.ComponentPresentButInactiveOnMiss={componentPresentButInactiveOnMissCount}"
                $"Mk2.ComponentMissingOnMiss={componentMissingOnMissCount}"
                $"Mk2.ComponentActiveAfterMiss={componentActiveAfterMissCount}"
                $"Mk2.ShotMatched={shotMatchedCount}"
                $"Mk2.ShotNotMatched={shotNotMatchedCount}"
                $"Mk2.MagnitudeAvailable={magnitudeAvailableCount}"
                $"Mk2.MagnitudeUnavailable={magnitudeUnavailableCount}"
                $"Mk2.SettlementAttempted={settlementAttemptedCount}"
                $"Mk2.SettlementApplied={settlementAppliedCount}"
                $"Mk2.SettlementNoChange={settlementNoChangeCount}"
                $"Last.Stage={lastStage}"
                $"Last.CulpritHandle={lastCulpritHandle}"
                $"Last.VictimHandle={lastVictimHandle}"
                $"Last.WeaponHash=0x{weaponHashText}"
                $"Last.ComponentHash=0x{componentHashText}"
                $"Last.ComponentName={lastComponentName}"
                $"Last.Effect={lastEffect}"
                $"Last.CandidateComponentHash=0x{candidateComponentHashText}"
                $"Last.CandidateComponentName={lastCandidateComponentName}"
                $"Last.CandidatePresent={lastCandidatePresent}"
                $"Last.CandidateActive={lastCandidateActive}"
                $"Last.CandidateSummary={lastCandidateSummary}"
                $"Last.BeforeHealth={lastBeforeHealth}"
                $"Last.BeforeArmour={lastBeforeArmour}"
                $"Last.AfterHealth={lastAfterHealth}"
                $"Last.AfterArmour={lastAfterArmour}"
                $"Last.ObservedDamage={lastObservedDamage}"
                $"Last.BaseDamage={lastBaseDamage}"
                $"Last.MagnitudeSource={lastMagnitudeSource}"
            |])

    member _.Advance(now: TimeSpan) =
        ObjectDisposedException.ThrowIf(disposed, typeof<Mk2Runtime>)
        synchronizePedPopulation()
        if continuityBroken() then
            resetAfterContinuityBreak()
        else
            processBufferedEvents now
            pruneShots now
            pruneShooters now
            pruneAimContexts()
            pruneVictims()
            pruneShotgunFallback now
            effects.Advance(now)
            damageContinuityRevision <- damageEvents.ContinuityRevision
            weaponContinuityRevision <- weaponEvents.ContinuityRevision
            damageDropped <- damages.DroppedCount
            aimDropped <- aimedAt.DroppedCount
            gunShotDropped <- gunShots.DroppedCount
            impactDropped <- impacts.DroppedCount

    member _.Clear() =
        if not disposed then
            disposed <- true
            damages.Dispose()
            aimedAt.Dispose()
            gunShots.Dispose()
            impacts.Dispose()
            damageHead <- None
            aimHead <- None
            gunShotHead <- None
            impactHead <- None
            shots.Clear()
            aimContexts.Clear()
            victimStates.Clear()
            resolvedRuleCache.Clear()
            shotgunFallback.Clear()
            for state in shooterStates.Values do state.Effects.Clear()
            shooterStates.Clear()
            effects.StopAll()
