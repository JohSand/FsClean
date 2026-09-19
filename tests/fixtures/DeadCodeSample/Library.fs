module DeadCodeSample.Library

// ---- functions -------------------------------------------------------------

let private helperUsedByLive x = x * 2
let usedFunction x = helperUsedByLive x + 1

// Dead: never referenced.
let unusedFunction x = x - 1

// Dead, and so is the helper that only it calls.
let private onlyUsedByDead x = x + 100
let unusedCaller x = onlyUsedByDead x

// Dead: a cycle that nothing reaches. The compiler's unused-value warnings can't see this.
let rec pingUnused n = if n <= 0 then 0 else pongUnused (n - 1)
and pongUnused n = if n <= 0 then 0 else pingUnused (n - 1)

// Dead, even though it carries a (neutral) attribute.
[<System.Obsolete>]
let obsoleteFunction () = ()

// ---- types -----------------------------------------------------------------

type UsedRecord = { Value: int }
type UnusedRecord = { Name: string }

// An augmentation of a dead type is dead with it.
type UnusedRecord with
    member r.Describe() = r.Name

type UsedUnion =
    | Alpha
    | Beta

type UnusedUnion =
    | Gamma
    | Delta

exception UsedError of string
exception UnusedError of string

type Calculator() =
    member _.Add(a: int, b: int) = a + b

    // Dead member of a live type.
    member _.NeverCalled(a: int) = a

    // Live whenever Calculator is: reached through virtual dispatch, not a named use.
    override _.ToString() = "Calculator"

    interface System.IDisposable with
        member _.Dispose() = ()

type UnusedClass() =
    member _.Anything = 42

// ---- modules and patterns --------------------------------------------------

module Nested =
    let usedNested = 1
    let unusedNested = 2

let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd

// ---- members the compiler calls without a named use ------------------------

type TraceBuilder() =
    member _.Bind(x: int, f: int -> int) = f x
    member _.Return(x: int) = x

    // `and!` and a trailing `return!` desugar to these; nothing names them.
    member _.Bind2(a: int, b: int, f: int * int -> int) = f (a, b)
    member _.ReturnFromFinal(x: int) = x

    // Dead member of a live builder.
    member _.NotBuilderRelated() = 0

let trace = TraceBuilder()

module BuilderExtensions =
    type TraceBuilder with
        // Extends a type from this code. Nothing names the block, but `for ... in` calls `For`, so
        // it stays alive for as long as TraceBuilder does.
        member _.For(items: int list, f: int -> int) = List.sumBy f items

module Extensions =
    type System.Text.StringBuilder with
        // Called by `for ... in`, but the compiler doesn't report a use. The block extends a type
        // from outside this code, so nobody in view can say who relies on it.
        member sb.GetEnumerator() = sb.ToString().GetEnumerator()

        // Dead: a plain extension member nothing calls.
        member sb.UnusedShout() = sb.ToString().ToUpper()

// ---- initializers with effects ---------------------------------------------

// Unused, but the initializer runs as part of this file's static initialization, so deleting it
// can change behaviour. Reported for review rather than treated as dead.
let announcedAtStartup = printfn "Library loaded"
