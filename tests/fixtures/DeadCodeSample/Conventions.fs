namespace FsCheck

/// Stands in for FsCheck's type of this name, which the analysis recognises.
type Arbitrary<'a> = { Sample: 'a }

namespace DeadCodeSample

module Conventions =
    /// ASP.NET calls Invoke by reflection (`UseMiddleware<T>()`); nothing in the code names it.
    type Middleware(next: obj) =
        member _.Invoke(context: int) = context + (next :?> int)

    /// FsCheck registers static members returning Arbitrary<_> from a type it's given, by reflection.
    type Generators =
        static member GenInt: FsCheck.Arbitrary<int> = { Sample = 1 }
