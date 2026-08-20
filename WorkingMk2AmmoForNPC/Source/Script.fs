namespace WorkingMk2AmmoForNPC.Source

open System
open System.Threading.Tasks
open CommunityScriptHookVDotNetCore.Source
open StandardGameOperations.Source

[<Sealed>]
type WorkingMk2AmmoForNPCScript() =
    inherit Script4()

    let mutable runtime : Mk2Runtime option = None

    override _.OnStartAsync(context: ScriptStartContext) =
        let gameOperations =
            context.Services.GetRequired<IStandardGameOperations>()

        if not gameOperations.Game.IsSupported then
            raise (
                InvalidOperationException(
                    "WorkingMk2AmmoForNPC requires a supported GTA V process."))

        runtime <-
            Some (
                Mk2Runtime(
                    GameAccess(gameOperations)))

        ValueTask.CompletedTask

    override _.OnTick(context: ScriptTickContext) =
        match runtime with
        | Some value ->
            value.Advance(
                context.HostFrameIndex,
                context.ElapsedTime)
        | None -> ()

    override _.OnStopAsync(_context: ScriptStopContext) =
        match runtime with
        | Some value -> value.Clear()
        | None -> ()
        runtime <- None
        ValueTask.CompletedTask