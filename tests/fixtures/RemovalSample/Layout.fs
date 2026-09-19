module RemovalSample.Layout

// ---- lets ------------------------------------------------------------------

/// Documented, commented and attributed, and dead: every line of it goes.
// A plain comment directly above is about it too.
[<System.Obsolete("nope")>]
let documentedDead x = x + 1

let live1 x = x * 2 // a trailing comment on a live line stays

let deadWithTrailing = 1 // this one goes with its line

// ---- chains ----------------------------------------------------------------

// Only the last declaration is dead: it can go, from its `and`.
let rec evenLive n = if n = 0 then true else oddLive (n - 1)
and oddLive n = if n = 0 then false else evenLive (n - 1)
and deadTail n = n

// The first declaration is dead but a later one is live: its `let rec` can't move, so it stays.
let rec deadHead n = n
and liveTail n = n + 1

// All of it is dead: it goes as one.
let rec deadA n = deadB n
and deadB n = deadA n

type LiveNode = { Next: LiveNode option }

and DeadLeaf = { Value: int }

// ---- members ---------------------------------------------------------------

type Holder() =
    // The only member, and dead: removing it would leave the type empty, so it stays.
    member _.OnlyDead = 1

type Pair() =
    member _.Live = 1
    member _.Dead = 2

// ---- modules ---------------------------------------------------------------

// Everything in it is dead, so the module can't stay behind empty: it goes too.
module AllDead =
    let a = 1
    let b x = x

// Only the inner module is dead: the outer one keeps its live declaration.
module Outer =
    module InnerDead =
        let c = 3

    let liveOuter = 4

// Emptying the inner module empties this one as well.
module OuterDead =
    module InnerDead2 =
        let d = 5

// The extension block names no live member, so it's dead, and so is the helper only it uses.
module Ext =
    let private helper x = x + 1

    type System.String with
        member s.Shout1() = helper 1
        member s.Shout2() = helper 2
