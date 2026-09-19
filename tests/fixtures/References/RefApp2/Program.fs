module RefApp2.Program

[<EntryPoint>]
let main _ =
    // RefCore reaches this project only through RefMiddle. Nothing of RefMiddle's own is used.
    printfn "%d" (RefCore.Core.used 2)
    0
