module RefApp.Program

// Dead: nothing calls it. RefUtil is only needed by this, so it's unneeded once this is gone.
let private deadCaller x = RefUtil.Util.helper x

[<EntryPoint>]
let main _ =
    // RefCore is used directly. RefMiddle is never used, and RefCore doesn't need it to be reached.
    printfn "%d" (RefCore.Core.used 1)
    0
