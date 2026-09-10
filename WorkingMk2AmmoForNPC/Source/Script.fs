namespace WorkingMk2AmmoForNPC.Source

open System
open System.IO
open System.Threading.Tasks
open CommunityScriptHookVDotNetCore.Source
open LowLevelEvents.Source
open LocalNativeMemories.Source
open StandardGameOperations.Source

[<Sealed>]
type WorkingMk2AmmoForNPCScript() =
    inherit Script4()

    let mutable runtime : Mk2Runtime option = None
    let mutable diagnosticPath : string option = None
    let mutable lastDiagnosticPayload = String.Empty
    let mutable nextDiagnosticAt = TimeSpan.Zero

    let writeDiagnostic path payload =
        try
            let timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
            File.AppendAllText(
                path,
                $"[{timestamp}] {payload}{Environment.NewLine}")
        with
        | :? IOException
        | :? UnauthorizedAccessException -> ()

    let flushDiagnostic (value: Mk2Runtime) force =
        match diagnosticPath with
        | None -> ()
        | Some path ->
            let payload = value.CreateDiagnosticPayload()
            if force || not (String.Equals(payload, lastDiagnosticPayload, StringComparison.Ordinal)) then
                writeDiagnostic path payload
                lastDiagnosticPayload <- payload

    override _.OnStartAsync(context: ScriptStartContext) : Task =
        task {
            let gameOperations =
                context.Services.GetRequired<IStandardGameOperations>()
            let entityPools =
                context.Services.GetRequired<ILocalEntityPools>()
            let lowLevelEvents =
                context.Services.GetRequired<ILowLevelEventStream>()
            let damageEvents =
                context.Services.GetRequired<ILowLevelDamageStream>()
            let weaponEvents =
                context.Services.GetRequired<ILowLevelWeaponEventStream>()
            let environment =
                context.Services.GetRequired<IScriptEnvironment>()

            let! loaded =
                Configuration.loadAsync
                    environment.ScriptsDirectory
                    context.CancellationToken

            Configuration.writeDiagnostics
                environment.ScriptsDirectory
                loaded.Issues

            let path =
                Path.Combine(
                    Path.GetFullPath(environment.ScriptsDirectory),
                    "WorkingMk2AmmoForNPC.runtime.log")
            diagnosticPath <- Some path
            lastDiagnosticPayload <- String.Empty
            nextDiagnosticAt <- TimeSpan.Zero
            try
                File.WriteAllText(path, String.Empty)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

            runtime <-
                Some (
                    Mk2Runtime(
                        GameAccess(gameOperations),
                        entityPools,
                        loaded.Configuration,
                        lowLevelEvents,
                        damageEvents,
                        weaponEvents))
        } :> Task

    override _.OnTick(context: ScriptTickContext) =
        match runtime with
        | Some value ->
            value.Advance(context.ElapsedTime)
            if context.ElapsedTime >= nextDiagnosticAt then
                flushDiagnostic value false
                nextDiagnosticAt <- context.ElapsedTime + TimeSpan.FromSeconds(1.0)
        | None -> ()

    override _.OnStopAsync(_context: ScriptStopContext) : Task =
        match runtime with
        | Some value ->
            flushDiagnostic value true
            value.Clear()
        | None -> ()
        runtime <- None
        diagnosticPath <- None
        Task.CompletedTask
