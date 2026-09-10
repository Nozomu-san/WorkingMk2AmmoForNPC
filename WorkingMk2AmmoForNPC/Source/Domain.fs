namespace WorkingMk2AmmoForNPC.Source

[<RequireQualifiedAccess>]
type internal Mk2AmmoEffect =
    | Explosive
    | HollowPoint
    | ArmorPiercing
    | Incendiary
    | FullMetalJacket
    | Tracer

[<RequireQualifiedAccess>]
type internal HeavyRevolverTracerBoostMode =
    | Always = 0
    | Never = 1
    | MagazineQuota = 2

[<Struct>]
type internal TracerCadence =
    { FirstTracerShot: int
      RepeatInterval: int }
    member this.UsesFixedQuota =
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
        elif cadence.UsesFixedQuota then
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
type internal ComponentRule =
    { ComponentName: string
      ComponentHash: uint32
      Effect: Mk2AmmoEffect
      TracerCadence: TracerCadence
      ExplosionType: int }

[<Struct>]
type internal OriginalComponentBinding =
    { ComponentName: string
      Effect: Mk2AmmoEffect }