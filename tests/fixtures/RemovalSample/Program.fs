module RemovalSample.Program

open RemovalSample.Vanishing

[<EntryPoint>]
let main _ =
    printfn "%d" (Layout.live1 1)
    printfn "%b" (Layout.evenLive 4)
    printfn "%d" (Layout.liveTail 1)
    printfn "%A" ({ Next = None }: Layout.LiveNode)
    Layout.Holder() |> ignore
    printfn "%d" (Layout.Pair().Live)
    printfn "%d" (Crlf.liveCrlf 1 + Crlf.alsoLive 2)
    printfn "%d" Layout.Outer.liveOuter
    0
