namespace Sample

module Builders =
    type Box<'a> = Box of 'a list

    type Builder() =
        member _.Zero() = ()
        member _.Delay(f: unit -> unit) = f ()
        member _.Combine((), ()) = ()

    let build = Builder()

/// The `for` a `build { for ... }` runs is this extension. Nothing in the source names it, so the
/// compiler service can't see that the open providing it is used.
module BuilderExtensions =
    type Builders.Builder with
        member _.For(Builders.Box items, body: 'a -> unit) : unit = List.iter body items
