namespace RemovalSample.Vanishing

open System

// Everything in this namespace is dead, but Program.fs opens it. The F# compiler treats a namespace with
// nothing in it as not defined (the compiler service doesn't), so removing it all would break that `open`.
module Gone =
    let x = 1
    let y = DateTime.MinValue
