module DeadCodeSample.Program

open DeadCodeSample.Library

[<EntryPoint>]
let main _ =
    use calc = new Calculator()
    printfn "%d" (usedFunction (calc.Add(1, 2)))
    printfn "%A" { Value = 1 }
    printfn "%A" Alpha
    printfn "%d" Nested.usedNested

    printfn "%d" (trace { return 1 })
    printfn "%A" (List.sum [ Money.Make 1; Money.Make 2 ])

    match 3 with
    | Even -> printfn "even"
    | Odd -> printfn "odd"

    try
        raise (UsedError "boom")
    with UsedError message ->
        printfn "%s" message

    0
