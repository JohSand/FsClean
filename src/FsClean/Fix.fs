/// Applies removals to the files on disk, keeping only the ones the compiler accepts. A preview
/// does everything but write: the compiler is shown the edited files from memory.
///
/// Whatever the analysis says, a removal only stays if the projects still type-check without it.
/// The compiler service is the quick check, and it is not the compiler: it accepts some code the
/// compiler rejects. With `realBuild`, once the removals type-check, the projects are built with
/// `dotnet build` and whatever that rejects is set aside too.
///
/// When a batch fails, the compiler's errors usually name what is missing, and the removals with
/// those names are set aside at once; when they don't, the batch is split in half and each half
/// tried again. Either way one wrong finding costs that finding rather than the whole run. Dead declarations that use each other are never split apart:
/// removing a helper without its dead caller can't compile, and says nothing about the helper.
module FsClean.Fix

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.ExceptionServices
open System.Text
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.IO
open FsClean.Analysis
open FsClean.ProjectLoader

type Mode =
    /// Write the removals to the files.
    | Apply
    /// Work out and check the removals, and change nothing.
    | Preview

type Result =
    {
      Removed: Decl list
      /// Left in place, and why.
      Kept: (Decl * string) list
      Files: string list
      /// Each changed file with its text before and after.
      Edited: (string * string * string) list
    }

let private utf8Bom = [| 0xEFuy; 0xBBuy; 0xBFuy |]

let hasBom (bytes: byte[]) =
    bytes.Length >= 3 && bytes[0] = utf8Bom[0] && bytes[1] = utf8Bom[1] && bytes[2] = utf8Bom[2]

let isUtf16 (bytes: byte[]) =
    bytes.Length >= 2
    && ((bytes[0] = 0xFFuy && bytes[1] = 0xFEuy) || (bytes[0] = 0xFEuy && bytes[1] = 0xFFuy))

let decode (bytes: byte[]) =
    let start = if hasBom bytes then 3 else 0
    Encoding.UTF8.GetString(bytes, start, bytes.Length - start)

/// Keeps the byte order mark the file came with.
let encode (original: byte[]) (text: string) =
    let body = UTF8Encoding(false).GetBytes text

    if hasBom original then
        Array.append utf8Bom body
    else
        body

/// Serves some files' contents from memory, so the compiler can be asked about edits that were
/// never written. `stamps` say when each was last "written": a compiler kept between checks re-checks
/// only what a changed stamp tells it about.
type Overlay(files: IReadOnlyDictionary<string, byte[]>, stamps: IReadOnlyDictionary<string, DateTime>) =
    inherit DefaultFileSystem()

    override _.GetLastWriteTimeShim(fileName) =
        match stamps.TryGetValue fileName with
        | true, stamp -> stamp
        | _ -> base.GetLastWriteTimeShim fileName

    override _.OpenFileForReadShim(filePath, ?useMemoryMappedFile, ?shouldShadowCopy) : Stream =
        match files.TryGetValue filePath with
        | true, bytes -> new MemoryStream(bytes) :> Stream
        | _ -> base.OpenFileForReadShim(filePath, ?useMemoryMappedFile = useMemoryMappedFile, ?shouldShadowCopy = shouldShadowCopy)

/// The compiler's file system is one setting for the whole process, so previews take turns.
let previewLock = obj ()

let private quoted = Regex("'([^']+)'", RegexOptions.Compiled)

/// The names an error message puts in quotes: `interpret2` in "The value or constructor
/// 'interpret2' is not defined".
let private mentionedIn (error: string) =
    [ for m in quoted.Matches error -> m.Groups[1].Value ]

let private buildError = Regex(@"^\s*(.+?)\((\d+),(\d+)\): error (\S+): (.*?)(?: \[[^\]]+\])?\s*$", RegexOptions.Compiled)

/// What `dotnet build` reports for these projects as they are on disk, as `file(line,col): message`
/// with absolute paths.
let buildErrors (projectFiles: string list) : Async<string list> =
    async {
        let errors = ResizeArray<string>()

        for file in projectFiles do
            let directory = Path.GetDirectoryName file
            let info = ProcessStartInfo("dotnet", $"build \"{file}\" --nologo -v q")
            info.WorkingDirectory <- directory
            info.RedirectStandardOutput <- true
            info.RedirectStandardError <- true
            use build = Process.Start info
            let! output = build.StandardOutput.ReadToEndAsync() |> Async.AwaitTask
            let! error = build.StandardError.ReadToEndAsync() |> Async.AwaitTask
            build.WaitForExit()

            for line in (output + "\n" + error).Split '\n' do
                let m = buildError.Match line

                if m.Success then
                    let path = Path.GetFullPath(m.Groups[1].Value.Replace('\\', '/'), directory)
                    let text = $"{path}({m.Groups[2].Value},{m.Groups[3].Value}): {m.Groups[5].Value}"

                    if not (errors.Contains text) then
                        errors.Add text

        return Seq.toList errors
    }

/// What an error can call a declaration: its own name, the type around it, and the modules around
/// that. Removing something can leave its module or type empty and gone, and the compiler then
/// complains about the module or type, not the member.
let private namesOf (decl: Decl) =
    let segments = decl.Name.Split '.'

    let rec modules (container: Ast.Container option) =
        match container with
        | Some c -> c.Name :: modules c.Outer
        | None -> []

    [ yield Array.last segments
      if segments.Length > 1 then
          yield segments[segments.Length - 2]
      yield! modules decl.Extent.Container ]

let private runCore
    (checker: FSharpChecker)
    (realBuild: bool)
    (progress: string -> unit)
    (mode: Mode)
    (projects: Project list)
    (dead: Decl list)
    : Async<Result> =
    async {
        let originals =
            dead
            |> List.map (fun decl -> decl.Range.FileName)
            |> List.distinct
            |> List.map (fun file -> file, File.ReadAllBytes file)
            |> Map.ofList

        // Only UTF-8 is handled; a file in anything else is left alone rather than mangled.
        let unsupported, editable =
            dead |> List.partition (fun decl -> isUtf16 originals[decl.Range.FileName])

        // What each file should currently contain; in a preview, what it would.
        let current = Dictionary<string, byte[]>()
        originals |> Map.iter (fun file bytes -> current[file] <- bytes)
        let stamps = Dictionary<string, DateTime>()

        // One compiler for every check, and the caller's for the first: after an edit it re-checks from
        // the first file that changed, rather than the whole solution again.

        let read file = decode originals[file]

        /// Makes the files what removing `decls` gives, and every other file what it was.
        let apply (decls: Decl list) =
            let outcome = Removal.plan read decls

            for KeyValue(file, original) in originals do
                let wanted =
                    match Map.tryFind file outcome.Changes with
                    | Some text -> encode original text
                    | None -> original

                if wanted <> current[file] then
                    if mode = Apply then
                        File.WriteAllBytes(file, wanted)

                    current[file] <- wanted
                    stamps[file] <- DateTime.UtcNow

            outcome

        let check (decls: Decl list) =
            async {
                apply decls |> ignore

                let! errors =
                    match mode with
                    | Apply -> typeErrors checker projects
                    | Preview ->
                        async {
                            return
                                lock previewLock (fun () ->
                                    let real = FileSystemAutoOpens.FileSystem
                                    FileSystemAutoOpens.FileSystem <-
                                        Overlay(Dictionary<string, byte[]>(current), Dictionary<string, DateTime>(stamps))

                                    try
                                        typeErrors checker projects |> Async.RunSynchronously
                                    finally
                                        FileSystemAutoOpens.FileSystem <- real)
                        }

                // Every check leaves a whole compiler's worth of type-checked projects behind.
                GC.Collect()
                return errors
            }

        let rec solve (accepted: Decl list) (units: Decl list list) (rejected: (Decl list * string) list) =
            async {
                if List.isEmpty units then
                    return accepted, rejected
                else
                    let candidate = accepted @ List.concat units
                    progress $"checking {units.Length} removals ({candidate.Length - accepted.Length} declarations)..."
                    let clock = Diagnostics.Stopwatch.StartNew()
                    let! errors = check candidate
                    let took = $"{clock.Elapsed.TotalSeconds:F0}s"

                    if List.isEmpty errors then
                        progress $"  type-checks ({took})"
                        return candidate, rejected
                    else
                        progress $"  {errors.Length} errors ({took})"

                        // The removals the errors name, each with the first error that does.
                        let complaints =
                            units
                            |> List.choose (fun unit ->
                                let names = unit |> List.collect namesOf |> Set.ofList

                                errors
                                |> List.tryFind (fun error -> mentionedIn error |> List.exists names.Contains)
                                |> Option.map (fun error -> unit, error))

                        if not complaints.IsEmpty && complaints.Length < units.Length then
                            progress $"  setting aside the {complaints.Length} removals the errors name"
                            let named = complaints |> List.map fst
                            let rest = units |> List.filter (fun unit -> not (List.contains unit named))

                            let more =
                                complaints
                                |> List.map (fun (unit, error) -> unit, $"removing it doesn't type-check: {error}")

                            return! solve accepted rest (more @ rejected)
                        else
                            match units with
                            | [ single ] ->
                                let why = $"removing it doesn't type-check: {List.head errors}"
                                return accepted, (single, why) :: rejected
                            | _ ->
                                let left, right = List.splitAt (units.Length / 2) units
                                let! accepted, rejected = solve accepted left rejected
                                return! solve accepted right rejected
            }

        // Cancelling isn't an exception, so `with` wouldn't see it; `finally` does.
        let completed = ref false

        try
            let units =
                editable
                |> List.groupBy (fun decl -> decl.Component)
                |> List.sortBy fst
                |> List.map snd

            let! accepted, rejected = solve [] units []

            // The compiler service accepts what the compiler doesn't, now and then, so build for real.
            let rec confirm (accepted: Decl list) (rejected: (Decl list * string) list) =
                async {
                    if not realBuild || mode = Preview || List.isEmpty accepted then
                        return accepted, rejected
                    else
                        apply accepted |> ignore
                        progress "building the projects with dotnet build..."

                        let! errors = buildErrors (projects |> List.map (fun project -> project.File) |> List.distinct)

                        if List.isEmpty errors then
                            progress "  builds"
                            return accepted, rejected
                        else
                            progress $"  {errors.Length} errors"

                            let units = accepted |> List.groupBy (fun decl -> decl.Component) |> List.map snd

                            // An error names what's missing, or is in a file a removal edited.
                            let culprits =
                                units
                                |> List.choose (fun unit ->
                                    let names = unit |> List.collect namesOf |> Set.ofList
                                    let files = unit |> List.map (fun decl -> decl.Range.FileName) |> Set.ofList

                                    errors
                                    |> List.tryFind (fun error ->
                                        mentionedIn error |> List.exists names.Contains
                                        || files |> Set.exists (fun file -> error.StartsWith(file + "(")))
                                    |> Option.map (fun error -> unit, error))

                            if culprits.IsEmpty then
                                let why = $"the build fails and the errors don't say which removal: {List.head errors}"
                                return [], (accepted, why) :: rejected
                            else
                                progress $"  setting aside the {culprits.Length} removals the errors name or sit in"
                                let named = culprits |> List.map fst
                                let rest = units |> List.filter (fun unit -> not (List.contains unit named))

                                let more =
                                    culprits |> List.map (fun (unit, error) -> unit, $"the build rejects removing it: {error}")

                                return! confirm (List.concat rest) (more @ rejected)
                }

            let! accepted, rejected = confirm accepted rejected

            // Leave the files as the accepted set alone makes them, whatever was tried last.
            let outcome = apply accepted

            completed.Value <- true

            return
                { Removed = outcome.Removed
                  Kept =
                    (unsupported |> List.map (fun decl -> decl, "its file isn't UTF-8"))
                    @ outcome.Skipped
                    @ (rejected |> List.collect (fun (decls, why) -> decls |> List.map (fun decl -> decl, why)))
                  Files = outcome.Changes |> Map.toList |> List.map fst
                  Edited =
                    outcome.Changes
                    |> Map.toList
                    |> List.map (fun (file, text) ->
                        // The byte order mark is part of the file's first line.
                        let mark = if hasBom originals[file] then "\uFEFF" else ""
                        file, mark + read file, mark + text) }
        finally
            // Put every file back when the run fails or is stopped.
            if not completed.Value then
                apply [] |> ignore
    }


/// Like `run`, telling `progress` what it's doing.
let runWith
    (checker: FSharpChecker)
    (progress: string -> unit)
    (mode: Mode)
    (projects: Project list)
    (dead: Decl list)
    : Async<Result> =
    runCore checker false progress mode projects dead

/// Like `runWith`, and once the removals type-check also builds the projects for real with
/// `dotnet build`, setting aside whatever that rejects. Ignored for a preview, which writes nothing.
let runBuilt
    (checker: FSharpChecker)
    (progress: string -> unit)
    (mode: Mode)
    (projects: Project list)
    (dead: Decl list)
    : Async<Result> =
    runCore checker true progress mode projects dead

let run (mode: Mode) (projects: Project list) (dead: Decl list) : Async<Result> =
    runWith (createChecker projects) ignore mode projects dead
