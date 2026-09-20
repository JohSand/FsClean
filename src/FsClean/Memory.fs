/// Stops a run before the kernel's OOM killer does. A kill can't be caught, and in the middle of a
/// `--fix` it leaves the removals being tried in the files; giving up while there is still memory
/// lets the run put every file back and say why.
module FsClean.Memory

open System
open System.IO
open System.Threading

/// Raised in place of the run when the machine is about to run out of memory.
exception Exhausted of availableBytes: int64

let private mib (bytes: int64) = bytes / 1048576L

/// A value from a `Key:   123 kB` line of /proc/meminfo, in bytes.
let private meminfo (key: string) =
    try
        File.ReadLines "/proc/meminfo"
        |> Seq.tryPick (fun line ->
            if line.StartsWith(key + ":") then
                line.Substring(key.Length + 1).Trim().Split(' ').[0] |> int64 |> Some
            else
                None)
        |> Option.map (fun kilobytes -> kilobytes * 1024L)
    with _ ->
        None

/// What's left of the container's limit, when there is one (cgroup v2). Page cache is counted as
/// used by the cgroup, but the kernel gives it back before it kills anything.
let private cgroupHeadroom () =
    try
        let limitText = (File.ReadAllText "/sys/fs/cgroup/memory.max").Trim()

        if limitText = "max" then
            None
        else
            let used = File.ReadAllText("/sys/fs/cgroup/memory.current").Trim() |> int64

            let reclaimable =
                File.ReadLines "/sys/fs/cgroup/memory.stat"
                |> Seq.tryPick (fun line ->
                    match line.Split ' ' with
                    | [| "inactive_file"; bytes |] -> Some(int64 bytes)
                    | _ -> None)
                |> Option.defaultValue 0L

            Some(int64 limitText - used + reclaimable)
    with _ ->
        None

/// Memory that can still be had without something being killed, or None where it can't be told.
let availableBytes () : int64 option =
    let machine =
        if OperatingSystem.IsLinux() then
            meminfo "MemAvailable"
        else
            // As of the last collection, which is frequent while the compiler runs.
            let info = GC.GetGCMemoryInfo()

            if info.TotalAvailableMemoryBytes > 0L then
                Some(info.TotalAvailableMemoryBytes - info.MemoryLoadBytes)
            else
                None

    match machine, cgroupHeadroom () with
    | Some a, Some b -> Some(min a b)
    | Some a, None
    | None, Some a -> Some a
    | None, None -> None

/// How little available memory is too little: enough for the kernel and whatever else is running
/// to keep going while this run winds down. FSCLEAN_MEMORY_FLOOR_MB replaces it.
let private floor (total: int64) =
    match Int64.TryParse(Environment.GetEnvironmentVariable "FSCLEAN_MEMORY_FLOOR_MB") with
    | true, megabytes -> megabytes * 1048576L
    | _ -> max (512L * 1048576L) (total / 50L)

/// Watches the available memory from a thread of its own, since the threads doing the work may
/// be stuck inside the compiler for a minute. When it stays low even after a full collection, the
/// work is cancelled, and `Run` raises `Exhausted` in its place.
type Watch(log: string -> unit) =
    let cancellation = new CancellationTokenSource()
    let mutable tripped: int64 option = None
    let mutable stopped = false

    let total = max 0L (GC.GetGCMemoryInfo().TotalAvailableMemoryBytes)

    let isLow () =
        match availableBytes () with
        | Some bytes when bytes < floor total -> Some bytes
        | _ -> None

    let watch () =
        let mutable low = 0

        while not stopped && tripped.IsNone do
            Thread.Sleep 250

            match isLow () with
            | None -> low <- 0
            | Some _ when low = 0 ->
                // The runtime holds on to memory it has freed; hand it back before deciding.
                low <- 1
                GC.Collect(2, GCCollectionMode.Aggressive, true, true)
            | Some bytes ->
                low <- low + 1

                if low >= 3 then
                    tripped <- Some bytes
                    log $"fsclean: only {mib bytes} MB of memory left; stopping before the system kills the run."
                    cancellation.Cancel()

    do
        if total > 0L && (availableBytes ()).IsSome then
            Thread(watch, IsBackground = true, Name = "memory watch").Start()

    /// Runs the work, or raises `Exhausted` if it had to be stopped for want of memory. Files a
    /// cancelled `--fix` touched are put back on the way out.
    member _.Run(work: Async<'a>) : 'a =
        try
            Async.RunSynchronously(work, cancellationToken = cancellation.Token)
        with :? OperationCanceledException when tripped.IsSome ->
            raise (Exhausted tripped.Value)

    interface IDisposable with
        member _.Dispose() =
            stopped <- true
            cancellation.Dispose()

/// Watches for the rest of the run. `log` gets the warning as it happens.
let watch (log: string -> unit) = new Watch(log)
