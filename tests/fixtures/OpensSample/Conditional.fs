module Sample.Conditional

open System.IO

#if NEVER
let exists path = File.Exists path
#endif

let answer = 42
