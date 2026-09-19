module LibrarySample.Api

let private usedByPublic x = x + 1

// Public API: a root when this is a library, whether or not anything in view calls it.
let publicUsed x = usedByPublic x
let publicUnused x = x - 1

// Private and unreachable: dead either way.
let private neverCalled x = x * 2
