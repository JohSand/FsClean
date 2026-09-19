module RemovalSample.Crlf

let liveCrlf x = x + 1

/// Dead, in a file with CRLF line endings and a byte order mark.
let deadCrlf x = x - 1

let alsoLive x = x * 3
