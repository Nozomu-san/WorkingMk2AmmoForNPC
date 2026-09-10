namespace WorkingMk2AmmoForNPC.Source

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading
open StandardJsonConfiguration.Source

[<RequireQualifiedAccess>]
type internal IncendiaryIgnitionMode =
    | Always = 0
    | Never = 1
    | Weibull = 2

[<AllowNullLiteral; Sealed>]
type internal Mk2UserConfiguration() =
    [<JsonInclude; JsonPropertyName("RidiculousMode")>]
    member val RidiculousMode = false with get, set

    [<JsonInclude; JsonPropertyName("ExplosiveEnabled")>]
    member val ExplosiveEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("ExplosiveTheoreticalRadiusDamageEnabled")>]
    member val ExplosiveTheoreticalRadiusDamageEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("TracerDamageHandlerEnabled")>]
    member val TracerDamageHandlerEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("HollowPointDamageHandlerEnabled")>]
    member val HollowPointDamageHandlerEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("FullMetalJacketDamageHandlerEnabled")>]
    member val FullMetalJacketDamageHandlerEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("FullMetalJacketPaperDamageEnabled")>]
    member val FullMetalJacketPaperDamageEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("ArmorPiercingDamageHandlerEnabled")>]
    member val ArmorPiercingDamageHandlerEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("ArmorPiercingPaperDamageEnabled")>]
    member val ArmorPiercingPaperDamageEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("IncendiaryDamageHandlerEnabled")>]
    member val IncendiaryDamageHandlerEnabled = true with get, set

    [<JsonInclude; JsonPropertyName("HeavyRevolverTracerBoostMode")>]
    member val HeavyRevolverTracerBoostMode =
        HeavyRevolverTracerBoostMode.MagazineQuota with get, set

    [<JsonInclude; JsonPropertyName("IncendiaryIgnitionMode")>]
    member val IncendiaryIgnitionMode = IncendiaryIgnitionMode.Weibull with get, set

    [<JsonInclude; JsonPropertyName("IncendiaryIgnitionRate")>]
    member val IncendiaryIgnitionRate = 0.05 with get, set

    [<JsonInclude; JsonPropertyName("IncendiaryIgnitionFairness")>]
    member val IncendiaryIgnitionFairness = 2.0 with get, set

    [<JsonInclude; JsonPropertyName("HeavyRevolverTracerBoostRate")>]
    member val HeavyRevolverTracerBoostRate = 0.50 with get, set

    [<JsonInclude; JsonPropertyName("TracerDamageMultiplier")>]
    member val TracerDamageMultiplier = 2.0 with get, set

    [<JsonInclude; JsonPropertyName("FullMetalJacketIntermediateMultiplier")>]
    member val FullMetalJacketIntermediateMultiplier = 2.0 with get, set

    [<JsonInclude; JsonPropertyName("VehicleFmjPropagationRadius")>]
    member val VehicleFmjPropagationRadius = 1.50 with get, set

    [<JsonInclude; JsonPropertyName("IncendiaryVehicleEngineMultiplier")>]
    member val IncendiaryVehicleEngineMultiplier = 4.0 with get, set

    [<JsonInclude; JsonPropertyName("ExplosiveFallbackRadius")>]
    member val ExplosiveFallbackRadius = 3.0 with get, set

[<AllowNullLiteral; Sealed>]
type internal Mk2AddonPatch() =
    member val ExplosiveEnabled = Nullable<bool>() with get, set
    member val ExplosiveTheoreticalRadiusDamageEnabled = Nullable<bool>() with get, set
    member val TracerDamageHandlerEnabled = Nullable<bool>() with get, set
    member val HollowPointDamageHandlerEnabled = Nullable<bool>() with get, set
    member val FullMetalJacketDamageHandlerEnabled = Nullable<bool>() with get, set
    member val FullMetalJacketPaperDamageEnabled = Nullable<bool>() with get, set
    member val ArmorPiercingDamageHandlerEnabled = Nullable<bool>() with get, set
    member val ArmorPiercingPaperDamageEnabled = Nullable<bool>() with get, set
    member val IncendiaryDamageHandlerEnabled = Nullable<bool>() with get, set
    member val HeavyRevolverTracerBoostMode = Nullable<HeavyRevolverTracerBoostMode>() with get, set
    member val IncendiaryIgnitionMode = Nullable<IncendiaryIgnitionMode>() with get, set
    member val IncendiaryIgnitionRate = Nullable<double>() with get, set
    member val IncendiaryIgnitionFairness = Nullable<double>() with get, set
    member val HeavyRevolverTracerBoostRate = Nullable<double>() with get, set
    member val TracerDamageMultiplier = Nullable<double>() with get, set
    member val FullMetalJacketIntermediateMultiplier = Nullable<double>() with get, set
    member val VehicleFmjPropagationRadius = Nullable<double>() with get, set
    member val IncendiaryVehicleEngineMultiplier = Nullable<double>() with get, set
    member val ExplosiveFallbackRadius = Nullable<double>() with get, set

[<Struct>]
type internal Mk2BalanceSettings =
    { ExplosiveEnabled: bool
      ExplosiveTheoreticalRadiusDamageEnabled: bool
      TracerDamageHandlerEnabled: bool
      HollowPointDamageHandlerEnabled: bool
      FullMetalJacketDamageHandlerEnabled: bool
      FullMetalJacketPaperDamageEnabled: bool
      ArmorPiercingDamageHandlerEnabled: bool
      ArmorPiercingPaperDamageEnabled: bool
      IncendiaryDamageHandlerEnabled: bool
      HeavyRevolverTracerBoostMode: HeavyRevolverTracerBoostMode
      IncendiaryIgnitionMode: IncendiaryIgnitionMode
      IncendiaryIgnition: WeibullIgnitionProfile
      HeavyRevolverTracerBoostRate: double
      TracerDamageMultiplier: single
      FullMetalJacketIntermediateMultiplier: single
      VehicleFmjPropagationRadius: single
      IncendiaryVehicleEngineMultiplier: single
      ExplosiveFallbackRadius: single }

[<Struct>]
type internal Mk2EffectiveConfiguration =
    { RidiculousMode: bool
      Balance: Mk2BalanceSettings }

[<Struct>]
type internal Mk2ConfigurationLoadResult =
    { Configuration: Mk2EffectiveConfiguration
      Issues: JsonConfigurationIssue array }

[<RequireQualifiedAccess>]
module internal Configuration =
    [<Literal>]
    let private PrimaryFileName = "WorkingMk2AmmoForNPC.json"

    [<Literal>]
    let private AddonDirectoryName = "WorkingMk2AmmoForNPC.Addons"

    let private primaryPolicy =
        JsonConfigurationPolicy(
            RecursiveAddons = false,
            PrimaryWriteBack = PrimaryWriteBackMode.MissingOnly,
            StartWithDefault = true,
            MissingProvider = JsonProviderFailureMode.Skip,
            InvalidProvider = JsonProviderFailureMode.Skip,
            UnavailableProvider = JsonProviderFailureMode.Skip,
            MaximumConcurrentFiles = 4,
            FileTimeout = TimeSpan.FromSeconds 5.0,
            MaximumFileBytes = 4L * 1024L * 1024L)

    let private addonPolicy =
        JsonConfigurationPolicy(
            RecursiveAddons = true,
            PrimaryWriteBack = PrimaryWriteBackMode.Never,
            StartWithDefault = true,
            MissingProvider = JsonProviderFailureMode.Skip,
            InvalidProvider = JsonProviderFailureMode.Skip,
            UnavailableProvider = JsonProviderFailureMode.Skip,
            MaximumConcurrentFiles = 4,
            FileTimeout = TimeSpan.FromSeconds 5.0,
            MaximumFileBytes = 4L * 1024L * 1024L)

    let private createOptions () =
        let options =
            JsonSerializerOptions(
                JsonSerializerDefaults.General,
                AllowTrailingCommas = true,
                ReadCommentHandling = JsonCommentHandling.Skip,
                WriteIndented = true,
                UnmappedMemberHandling = JsonUnmappedMemberHandling.Disallow,
                DefaultIgnoreCondition = JsonIgnoreCondition.Never)
        options.Converters.Add(JsonStringEnumConverter(null, false))
        options

    let private chooseNullable (current: Nullable<'T>) (incoming: Nullable<'T>) =
        if incoming.HasValue then incoming else current

    let private mergeAddonPatch (current: Mk2AddonPatch) (incoming: Mk2AddonPatch) =
        if isNull incoming then
            current
        else
            let baseline = if isNull current then Mk2AddonPatch() else current
            let result = Mk2AddonPatch()
            result.ExplosiveEnabled <-
                chooseNullable baseline.ExplosiveEnabled incoming.ExplosiveEnabled
            result.ExplosiveTheoreticalRadiusDamageEnabled <-
                chooseNullable
                    baseline.ExplosiveTheoreticalRadiusDamageEnabled
                    incoming.ExplosiveTheoreticalRadiusDamageEnabled
            result.TracerDamageHandlerEnabled <-
                chooseNullable baseline.TracerDamageHandlerEnabled incoming.TracerDamageHandlerEnabled
            result.HollowPointDamageHandlerEnabled <-
                chooseNullable baseline.HollowPointDamageHandlerEnabled incoming.HollowPointDamageHandlerEnabled
            result.FullMetalJacketDamageHandlerEnabled <-
                chooseNullable
                    baseline.FullMetalJacketDamageHandlerEnabled
                    incoming.FullMetalJacketDamageHandlerEnabled
            result.FullMetalJacketPaperDamageEnabled <-
                chooseNullable
                    baseline.FullMetalJacketPaperDamageEnabled
                    incoming.FullMetalJacketPaperDamageEnabled
            result.ArmorPiercingDamageHandlerEnabled <-
                chooseNullable
                    baseline.ArmorPiercingDamageHandlerEnabled
                    incoming.ArmorPiercingDamageHandlerEnabled
            result.ArmorPiercingPaperDamageEnabled <-
                chooseNullable
                    baseline.ArmorPiercingPaperDamageEnabled
                    incoming.ArmorPiercingPaperDamageEnabled
            result.IncendiaryDamageHandlerEnabled <-
                chooseNullable
                    baseline.IncendiaryDamageHandlerEnabled
                    incoming.IncendiaryDamageHandlerEnabled
            result.HeavyRevolverTracerBoostMode <-
                chooseNullable
                    baseline.HeavyRevolverTracerBoostMode
                    incoming.HeavyRevolverTracerBoostMode
            result.IncendiaryIgnitionMode <-
                chooseNullable baseline.IncendiaryIgnitionMode incoming.IncendiaryIgnitionMode
            result.IncendiaryIgnitionRate <-
                chooseNullable baseline.IncendiaryIgnitionRate incoming.IncendiaryIgnitionRate
            result.IncendiaryIgnitionFairness <-
                chooseNullable baseline.IncendiaryIgnitionFairness incoming.IncendiaryIgnitionFairness
            result.HeavyRevolverTracerBoostRate <-
                chooseNullable baseline.HeavyRevolverTracerBoostRate incoming.HeavyRevolverTracerBoostRate
            result.TracerDamageMultiplier <-
                chooseNullable baseline.TracerDamageMultiplier incoming.TracerDamageMultiplier
            result.FullMetalJacketIntermediateMultiplier <-
                chooseNullable baseline.FullMetalJacketIntermediateMultiplier incoming.FullMetalJacketIntermediateMultiplier
            result.VehicleFmjPropagationRadius <-
                chooseNullable baseline.VehicleFmjPropagationRadius incoming.VehicleFmjPropagationRadius
            result.IncendiaryVehicleEngineMultiplier <-
                chooseNullable baseline.IncendiaryVehicleEngineMultiplier incoming.IncendiaryVehicleEngineMultiplier
            result.ExplosiveFallbackRadius <-
                chooseNullable baseline.ExplosiveFallbackRadius incoming.ExplosiveFallbackRadius
            result

    let private addIssue
        (issues: ResizeArray<JsonConfigurationIssue>)
        severity
        message =
        issues.Add(JsonConfigurationIssue(severity, message))

    let private validateRate path value strict =
        if not (Double.IsFinite(value)) ||
           (strict && (value <= 0.0 || value >= 1.0)) ||
           (not strict && (value < 0.0 || value > 1.0)) then
            Some path
        else
            None

    let private validateUser (value: Mk2UserConfiguration) =
        let issues = ResizeArray<JsonConfigurationIssue>()
        if isNull value then
            issues.ToArray() :> System.Collections.Generic.IReadOnlyList<JsonConfigurationIssue>
        else
            if not (Enum.IsDefined(typeof<HeavyRevolverTracerBoostMode>, value.HeavyRevolverTracerBoostMode)) then
                addIssue issues JsonConfigurationSeverity.Error "HeavyRevolverTracerBoostMode must be Always, Never, or MagazineQuota."
            if not (Enum.IsDefined(typeof<IncendiaryIgnitionMode>, value.IncendiaryIgnitionMode)) then
                addIssue issues JsonConfigurationSeverity.Error "IncendiaryIgnitionMode must be Always, Never, or Weibull."
            elif value.IncendiaryIgnitionMode = IncendiaryIgnitionMode.Weibull then
                match validateRate "IncendiaryIgnitionRate" value.IncendiaryIgnitionRate true with
                | Some path -> addIssue issues JsonConfigurationSeverity.Error $"{path} must be finite and strictly between 0 and 1."
                | None -> ()
                if not (Double.IsFinite(value.IncendiaryIgnitionFairness)) || value.IncendiaryIgnitionFairness < 1.0 then
                    addIssue issues JsonConfigurationSeverity.Error "IncendiaryIgnitionFairness must be finite and at least 1."
            if value.HeavyRevolverTracerBoostMode = HeavyRevolverTracerBoostMode.MagazineQuota then
                match validateRate "HeavyRevolverTracerBoostRate" value.HeavyRevolverTracerBoostRate true with
                | Some path -> addIssue issues JsonConfigurationSeverity.Error $"{path} must be finite and strictly between 0 and 1."
                | None -> ()
            for name, numeric in
                [ "TracerDamageMultiplier", value.TracerDamageMultiplier
                  "FullMetalJacketIntermediateMultiplier", value.FullMetalJacketIntermediateMultiplier
                  "VehicleFmjPropagationRadius", value.VehicleFmjPropagationRadius
                  "IncendiaryVehicleEngineMultiplier", value.IncendiaryVehicleEngineMultiplier
                  "ExplosiveFallbackRadius", value.ExplosiveFallbackRadius ] do
                if not (Double.IsFinite(numeric)) || numeric <= 0.0 then
                    addIssue issues JsonConfigurationSeverity.Error $"{name} must be finite and greater than 0."
            issues.ToArray() :> System.Collections.Generic.IReadOnlyList<JsonConfigurationIssue>

    let private validateAddon (value: Mk2AddonPatch) =
        let issues = ResizeArray<JsonConfigurationIssue>()
        if not (isNull value) then
            if value.HeavyRevolverTracerBoostMode.HasValue &&
               not (Enum.IsDefined(typeof<HeavyRevolverTracerBoostMode>, value.HeavyRevolverTracerBoostMode.Value)) then
                addIssue issues JsonConfigurationSeverity.Error "HeavyRevolverTracerBoostMode must be Always, Never, or MagazineQuota."
            if value.IncendiaryIgnitionMode.HasValue &&
               not (Enum.IsDefined(typeof<IncendiaryIgnitionMode>, value.IncendiaryIgnitionMode.Value)) then
                addIssue issues JsonConfigurationSeverity.Error "IncendiaryIgnitionMode must be Always, Never, or Weibull."
            let validateIgnitionDistribution =
                not value.IncendiaryIgnitionMode.HasValue ||
                value.IncendiaryIgnitionMode.Value = IncendiaryIgnitionMode.Weibull
            if validateIgnitionDistribution && value.IncendiaryIgnitionRate.HasValue then
                match validateRate "IncendiaryIgnitionRate" value.IncendiaryIgnitionRate.Value true with
                | Some path -> addIssue issues JsonConfigurationSeverity.Error $"{path} must be finite and strictly between 0 and 1."
                | None -> ()
            if validateIgnitionDistribution &&
               value.IncendiaryIgnitionFairness.HasValue &&
               (not (Double.IsFinite(value.IncendiaryIgnitionFairness.Value)) || value.IncendiaryIgnitionFairness.Value < 1.0) then
                addIssue issues JsonConfigurationSeverity.Error "IncendiaryIgnitionFairness must be finite and at least 1."
            let validateTracerDistribution =
                not value.HeavyRevolverTracerBoostMode.HasValue ||
                value.HeavyRevolverTracerBoostMode.Value = HeavyRevolverTracerBoostMode.MagazineQuota
            if validateTracerDistribution && value.HeavyRevolverTracerBoostRate.HasValue then
                match validateRate "HeavyRevolverTracerBoostRate" value.HeavyRevolverTracerBoostRate.Value true with
                | Some path -> addIssue issues JsonConfigurationSeverity.Error $"{path} must be finite and strictly between 0 and 1."
                | None -> ()
            let positives =
                [ "TracerDamageMultiplier", value.TracerDamageMultiplier
                  "FullMetalJacketIntermediateMultiplier", value.FullMetalJacketIntermediateMultiplier
                  "VehicleFmjPropagationRadius", value.VehicleFmjPropagationRadius
                  "IncendiaryVehicleEngineMultiplier", value.IncendiaryVehicleEngineMultiplier
                  "ExplosiveFallbackRadius", value.ExplosiveFallbackRadius ]
            for name, numeric in positives do
                if numeric.HasValue &&
                   (not (Double.IsFinite(numeric.Value)) || numeric.Value <= 0.0) then
                    addIssue issues JsonConfigurationSeverity.Error $"{name} must be finite and greater than 0."
        issues.ToArray() :> System.Collections.Generic.IReadOnlyList<JsonConfigurationIssue>

    let private valueOr fallback (value: Nullable<double>) =
        if value.HasValue then value.Value else fallback

    let private boolOr fallback (value: Nullable<bool>) =
        if value.HasValue then value.Value else fallback

    let private strictRateOr fallback value =
        if Double.IsFinite(value) && value > 0.0 && value < 1.0 then value else fallback

    let private unitRateOr fallback value =
        if Double.IsFinite(value) && value >= 0.0 && value <= 1.0 then value else fallback

    let private atLeastOneOr fallback value =
        if Double.IsFinite(value) && value >= 1.0 then value else fallback

    let private positiveOr fallback value =
        if Double.IsFinite(value) && value > 0.0 then value else fallback

    let private buildBalance (user: Mk2UserConfiguration) (addon: Mk2AddonPatch) =
        let patch = if isNull addon then Mk2AddonPatch() else addon
        let requestedTracerMode =
            if patch.HeavyRevolverTracerBoostMode.HasValue then
                patch.HeavyRevolverTracerBoostMode.Value
            else
                user.HeavyRevolverTracerBoostMode
        let tracerMode =
            if Enum.IsDefined(typeof<HeavyRevolverTracerBoostMode>, requestedTracerMode) then
                requestedTracerMode
            else
                HeavyRevolverTracerBoostMode.MagazineQuota
        let requestedMode =
            if patch.IncendiaryIgnitionMode.HasValue then patch.IncendiaryIgnitionMode.Value
            else user.IncendiaryIgnitionMode
        let ignitionMode =
            if Enum.IsDefined(typeof<IncendiaryIgnitionMode>, requestedMode) then requestedMode
            else IncendiaryIgnitionMode.Weibull
        let ignitionRate =
            valueOr user.IncendiaryIgnitionRate patch.IncendiaryIgnitionRate
            |> strictRateOr 0.05
        let ignitionFairness =
            valueOr user.IncendiaryIgnitionFairness patch.IncendiaryIgnitionFairness
            |> atLeastOneOr 2.0
        { ExplosiveEnabled =
              boolOr user.ExplosiveEnabled patch.ExplosiveEnabled
          ExplosiveTheoreticalRadiusDamageEnabled =
              boolOr
                  user.ExplosiveTheoreticalRadiusDamageEnabled
                  patch.ExplosiveTheoreticalRadiusDamageEnabled
          TracerDamageHandlerEnabled =
              boolOr user.TracerDamageHandlerEnabled patch.TracerDamageHandlerEnabled
          HollowPointDamageHandlerEnabled =
              boolOr
                  user.HollowPointDamageHandlerEnabled
                  patch.HollowPointDamageHandlerEnabled
          FullMetalJacketDamageHandlerEnabled =
              boolOr
                  user.FullMetalJacketDamageHandlerEnabled
                  patch.FullMetalJacketDamageHandlerEnabled
          FullMetalJacketPaperDamageEnabled =
              boolOr
                  user.FullMetalJacketPaperDamageEnabled
                  patch.FullMetalJacketPaperDamageEnabled
          ArmorPiercingDamageHandlerEnabled =
              boolOr
                  user.ArmorPiercingDamageHandlerEnabled
                  patch.ArmorPiercingDamageHandlerEnabled
          ArmorPiercingPaperDamageEnabled =
              boolOr
                  user.ArmorPiercingPaperDamageEnabled
                  patch.ArmorPiercingPaperDamageEnabled
          IncendiaryDamageHandlerEnabled =
              boolOr
                  user.IncendiaryDamageHandlerEnabled
                  patch.IncendiaryDamageHandlerEnabled
          HeavyRevolverTracerBoostMode = tracerMode
          IncendiaryIgnitionMode = ignitionMode
          IncendiaryIgnition = WeibullIgnitionProfile.create ignitionRate ignitionFairness
          HeavyRevolverTracerBoostRate =
              valueOr user.HeavyRevolverTracerBoostRate patch.HeavyRevolverTracerBoostRate
              |> strictRateOr 0.50
          TracerDamageMultiplier =
              valueOr user.TracerDamageMultiplier patch.TracerDamageMultiplier
              |> positiveOr 2.0
              |> single
          FullMetalJacketIntermediateMultiplier =
              valueOr user.FullMetalJacketIntermediateMultiplier patch.FullMetalJacketIntermediateMultiplier
              |> positiveOr 2.0
              |> single
          VehicleFmjPropagationRadius =
              valueOr user.VehicleFmjPropagationRadius patch.VehicleFmjPropagationRadius
              |> positiveOr 1.50
              |> single
          IncendiaryVehicleEngineMultiplier =
              valueOr user.IncendiaryVehicleEngineMultiplier patch.IncendiaryVehicleEngineMultiplier
              |> positiveOr 4.0
              |> single
          ExplosiveFallbackRadius =
              valueOr user.ExplosiveFallbackRadius patch.ExplosiveFallbackRadius
              |> positiveOr 3.0
              |> single }

    [<Sealed>]
    type private UserConfigurationModule() =
        interface IJsonConfigurationModule<Mk2UserConfiguration> with
            member _.DataType = "WorkingMk2AmmoForNPC.Configuration"
            member _.CreateDefault() = Mk2UserConfiguration()
            member _.Merge(_current, incoming) =
                if isNull incoming then Mk2UserConfiguration() else incoming
            member _.Validate(value) = validateUser value

    [<Sealed>]
    type private AddonConfigurationModule() =
        interface IJsonConfigurationModule<Mk2AddonPatch> with
            member _.DataType = "WorkingMk2AmmoForNPC.Addons"
            member _.CreateDefault() = Mk2AddonPatch()
            member _.Merge(current, incoming) = mergeAddonPatch current incoming
            member _.Validate(value) = validateAddon value

    let writeDiagnostics (scriptsDirectory: string) (issues: JsonConfigurationIssue array) =
        if issues.Length <> 0 then
            try
                let path =
                    Path.Combine(
                        Path.GetFullPath(scriptsDirectory),
                        "WorkingMk2AmmoForNPC.config.log")
                let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
                let lines =
                    issues
                    |> Array.map (fun issue ->
                        $"[{timestamp}] [{issue.Severity}] {issue.Message}")
                File.AppendAllLines(path, lines)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    let loadAsync
        (scriptsDirectory: string)
        (cancellationToken: CancellationToken) =
        task {
            ArgumentException.ThrowIfNullOrWhiteSpace(scriptsDirectory)
            let root = Path.GetFullPath(scriptsDirectory)
            let primaryLayout = JsonConfigurationLayout(Path.Combine(root, PrimaryFileName))
            let addonLayout = JsonAddonLayout(Path.Combine(root, AddonDirectoryName))
            let userContract = JsonContract<Mk2UserConfiguration>(Options = createOptions())
            let addonContract = JsonContract<Mk2AddonPatch>(Options = createOptions())
            let! user =
                StandardJsonConfiguration.LoadAsync<Mk2UserConfiguration>(
                    primaryLayout,
                    UserConfigurationModule(),
                    userContract,
                    primaryPolicy,
                    cancellationToken)
            and! addons =
                StandardJsonConfiguration.LoadAsync<Mk2AddonPatch>(
                    addonLayout,
                    AddonConfigurationModule(),
                    addonContract,
                    addonPolicy,
                    cancellationToken)
            let userValue = if isNull user.Value then Mk2UserConfiguration() else user.Value
            let addonValue = if isNull addons.Value then Mk2AddonPatch() else addons.Value
            return
                { Configuration =
                      { RidiculousMode = userValue.RidiculousMode
                        Balance = buildBalance userValue addonValue }
                  Issues =
                      Array.append
                          (user.Issues |> Seq.toArray)
                          (addons.Issues |> Seq.toArray) }
        }