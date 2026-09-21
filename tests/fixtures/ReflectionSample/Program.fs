module Sample.Reflected

open System
open System.Diagnostics.CodeAnalysis

/// Everything of it is reached by reflection, private members and static ones included.
[<DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.All)>]
type AllOfIt() =
    member _.AProperty = 1
    member _.AMethod() = 2
    static member AStaticMethod() = 3
    member private _.APrivateMethod() = 4
    member _.AnEvent = Event<int>().Publish

/// Only the public properties are: the rest is dead as usual.
[<DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicProperties)>]
type OnlyProperties() =
    member _.ReachedProperty = 1
    member _.UnreachedMethod() = 2
    member private _.UnreachedPrivateProperty = 3

/// Flags combine.
[<DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.PublicMethods ||| DynamicallyAccessedMemberTypes.NonPublicMethods)>]
type AnyMethods() =
    member _.ReachedMethod() = 1
    member private _.ReachedPrivateMethod() = 2
    member _.UnreachedProperty = 3

/// Any other attribute keeps the type it's on, and nothing inside it.
type MarkerAttribute() =
    inherit Attribute()

[<Marker>]
type MarkedOnly() =
    member _.NotCoveredByTheMarker() = 1

type Plain() =
    member _.NotReached() = 1

[<EntryPoint>]
let main _ = 0
