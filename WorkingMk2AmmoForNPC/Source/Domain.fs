namespace WorkingMk2AmmoForNPC.Source

open System
open StandardGameOperations.Source

[<RequireQualifiedAccess>]
type internal Mk2AmmoEffect =
    | Explosive
    | HollowPoint
    | ArmorPiercing
    | Incendiary
    | FullMetalJacket
    | Tracer

[<Struct>]
type internal CriticalShotProfile =
    { ChancePercent: int
      DamageMultiplier: single }
    member this.IsEnabled =
        this.ChancePercent > 0 &&
        Single.IsFinite(this.DamageMultiplier) &&
        this.DamageMultiplier > 1.0f

[<RequireQualifiedAccess>]
module internal CriticalShotProfile =
    let create chancePercent damageMultiplier =
        if chancePercent <= 0 ||
           not (Single.IsFinite(damageMultiplier)) ||
           damageMultiplier <= 1.0f then
            Unchecked.defaultof<CriticalShotProfile>
        else
            { ChancePercent = min 100 chancePercent
              DamageMultiplier = damageMultiplier }

[<Struct>]
type internal TracerCadence =
    { FirstTracerShot: int
      RepeatInterval: int }
    member this.IsEveryShotRandomDoubleException =
        this.FirstTracerShot = 1 && this.RepeatInterval = 0

[<RequireQualifiedAccess>]
module internal TracerCadence =
    let defaultValue =
        { FirstTracerShot = 0
          RepeatInterval = 1 }

    let create firstTracerShot repeatInterval =
        if firstTracerShot = 1 && repeatInterval = 0 then
            { FirstTracerShot = 1; RepeatInterval = 0 }
        elif firstTracerShot < 0 ||
             repeatInterval <= 0 ||
             firstTracerShot >= repeatInterval then
            defaultValue
        else
            { FirstTracerShot = firstTracerShot
              RepeatInterval = repeatInterval }

    let isTracerShot (oneBasedShotNumber: int) (cadence: TracerCadence) =
        if oneBasedShotNumber <= 0 then
            false
        elif cadence.IsEveryShotRandomDoubleException then
            true
        else
            let normalized =
                if cadence.RepeatInterval > 0 &&
                   cadence.FirstTracerShot >= 0 &&
                   cadence.FirstTracerShot < cadence.RepeatInterval then
                    cadence
                else
                    defaultValue

            if normalized.FirstTracerShot = 0 then
                oneBasedShotNumber % normalized.RepeatInterval = 0
            else
                oneBasedShotNumber >= normalized.FirstTracerShot &&
                (oneBasedShotNumber - normalized.FirstTracerShot) %
                    normalized.RepeatInterval = 0

[<Struct>]
type internal AmmoRule =
    { WeaponName: string
      ComponentName: string
      AmmoTypeName: string
      WeaponHash: uint32
      ComponentHash: uint32
      AmmoTypeHash: uint32
      Effect: Mk2AmmoEffect
      CriticalShot: CriticalShotProfile }

[<Struct>]
type internal OriginalNameRule =
    { WeaponName: string
      ComponentName: string
      AmmoTypeName: string
      Effect: Mk2AmmoEffect }

[<Struct>]
type internal FullMetalJacketVehicleEffect =
    { Applied: bool
      TransferableDirectHealthLoss: single }

[<Sealed>]
type internal ExplosiveSettlement(
    baseline: EngagementBaseline,
    rule: AmmoRule,
    resolveAtHostFrame: uint64) =
    member _.Baseline = baseline
    member _.Rule = rule
    member _.ResolveAtHostFrame = resolveAtHostFrame