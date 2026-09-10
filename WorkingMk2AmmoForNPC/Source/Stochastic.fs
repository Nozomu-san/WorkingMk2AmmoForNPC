namespace WorkingMk2AmmoForNPC.Source

open System

[<Struct>]
type internal WeibullIgnitionProfile =
    { TargetRate: double
      Fairness: double
      Q: double }

[<RequireQualifiedAccess>]
module internal WeibullIgnitionProfile =
    let private expectedTrials q fairness =
        let mutable total = 1.0
        let mutable n = 1
        let mutable continueLoop = true
        while n < 65536 && continueLoop do
            let survival = Math.Pow(q, Math.Pow(double n, fairness))
            if not (Double.IsFinite(survival)) || survival <= 1.0e-14 then
                continueLoop <- false
            else
                total <- total + survival
                n <- n + 1
        total

    let private calibrateQ targetRate fairness =
        let desired = 1.0 / targetRate
        let mutable low = 0.0
        let mutable high = 1.0 - 1.0e-15
        for _iteration in 0 .. 79 do
            let middle = (low + high) * 0.5
            if expectedTrials middle fairness < desired then
                low <- middle
            else
                high <- middle
        (low + high) * 0.5

    let create targetRate fairness =
        if not (Double.IsFinite(targetRate)) ||
           targetRate <= 0.0 || targetRate >= 1.0 then
            invalidArg (nameof targetRate) "Ignition rate must be strictly between 0 and 1."
        if not (Double.IsFinite(fairness)) || fairness < 1.0 then
            invalidArg (nameof fairness) "Ignition fairness must be finite and at least 1."
        { TargetRate = targetRate
          Fairness = fairness
          Q = calibrateQ targetRate fairness }

    let withOverrides
        (targetRate: Nullable<double>)
        (fairness: Nullable<double>)
        (current: WeibullIgnitionProfile) =
        create
            (if targetRate.HasValue then targetRate.Value else current.TargetRate)
            (if fairness.HasValue then fairness.Value else current.Fairness)

[<Sealed>]
type internal WeibullIgnitionState() =
    let mutable failures = 0

    member _.Reset() = failures <- 0

    member _.TryNext(profile: WeibullIgnitionProfile) =
        let n = failures + 1
        let upper = Math.Pow(double n, profile.Fairness)
        let lower = Math.Pow(double (n - 1), profile.Fairness)
        let survival =
            Math.Exp(Math.Log(profile.Q) * (upper - lower))
        let rawHazard = 1.0 - survival
        let hazard =
            if not (Double.IsFinite(rawHazard)) || rawHazard >= 1.0 then
                Math.BitDecrement(1.0)
            elif rawHazard <= 0.0 then
                Double.Epsilon
            else
                rawHazard
        if Random.Shared.NextDouble() < hazard then
            failures <- 0
            true
        else
            failures <- failures + 1
            false

[<Sealed>]
type internal MagazineQuotaState() =
    let mutable slots : bool array = Array.empty
    let mutable nextIndex = 0

    member _.Reset(clipSize: int, rate: double) =
        let size = max 0 clipSize
        let normalizedRate =
            if Double.IsFinite(rate) && rate > 0.0 && rate < 1.0 then
                rate
            else
                0.50
        slots <- Array.zeroCreate size
        nextIndex <- 0
        if size = 1 then
            slots[0] <- Random.Shared.NextDouble() < normalizedRate
        elif size > 1 then
            let roundedQuota =
                Math.Round(
                    double size * normalizedRate,
                    MidpointRounding.AwayFromZero)
                |> int
            let quota = Math.Clamp(roundedQuota, 1, size - 1)
            let indices = Array.init size id
            for i in 0 .. quota - 1 do
                let j = Random.Shared.Next(i, size)
                let tmp = indices[i]
                indices[i] <- indices[j]
                indices[j] <- tmp
                slots[indices[i]] <- true

    member _.TakeNext() =
        if slots.Length = 0 then
            false
        else
            if nextIndex >= slots.Length then
                nextIndex <- 0
            let value = slots[nextIndex]
            nextIndex <- nextIndex + 1
            value