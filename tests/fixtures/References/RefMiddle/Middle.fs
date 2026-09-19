module RefMiddle.Middle

// Public API of a library, so a root by default. Whole-program mode finds nothing calling it.
let middleFn x = RefCore.Core.used x
