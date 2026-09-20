/// Unused `open`s. A file-local question, so it needs none of the dead-code analysis: the compiler
/// service already says which opens nothing in a file resolves through.
///
/// That answer is an editor's, and editors get it wrong: an open can be needed for an operator, an
/// extension member or an active pattern without any name pointing at it. So `fix` checks it the way
/// dead-code removal is projectResults, and a removal only stays if
///  - the projects still type-check, and
///  - every name in the edited files still resolves to the same symbol. Two opens can offer the same
///    name; take away the one that wins and the code still compiles, and does something else.
///
/// An open only affects its own file, so each file is judged on its own: its opens are tried against
/// that file alone, in memory, a handful of files at a time, and a failing batch is halved until the
/// culprit is found. That costs a file's check per try. What one file's check can't see is a change in
/// what the file offers other files, so the opens that passed are then checked together against the
/// whole solution, which is the one expensive step and usually the only round. Only when that fails,
/// or when a file can't be judged alone, are there more rounds, and those blame by file as before.
///
/// Memory stays bounded: a file's check keeps two small arrays and drops the rest, the solution-wide
/// use lists are never built, and the compiler's results are dropped and collected before every
/// whole-solution check, so a check's results never sit beside the previous one's.
module FsClean.Opens

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.IO
open FSharp.Compiler.Text
open FsClean.ProjectLoader

/// An `open` the compiler service says nothing in its file needs.
type Candidate =
    { File: string
      Range: range
      /// The source text of the open's line, for the report.
      Line: string }

/// Generated files (the build's AssemblyInfo, `.g.fs`) are rewritten on every build; editing one is pointless.
let private isGenerated (file: string) =
    let normalized = file.Replace('\\', '/')
    normalized.Contains "/obj/" || normalized.EndsWith ".g.fs"

/// The opens the compiler service considers unused, in every source file of `projects`. A file shared
/// between projects, or compiled once per target framework, is reported once. `Error` carries the
/// type errors, when the projects don't type-check: the answer would mean nothing.
let findWith
    (log: string -> unit)
    (checker: FSharpChecker)
    (projects: Project list)
    : Async<Result<Candidate list, string list>> =
    async {
        let errors = ResizeArray<string>()
        let found = Dictionary<string * range, Candidate>(HashIdentity.Structural)

        for project in projects do
            let clock = Diagnostics.Stopwatch.StartNew()
            let! results = checker.ParseAndCheckProject project.Options
            errors.AddRange(Analysis.errorsIn results)
            log $"  type-checked {References.name project.File}: {clock.Elapsed.TotalSeconds:F1}s"
            clock.Restart()

            if errors.Count = 0 then
                let files = Analysis.sourceFiles project.Options |> Array.filter (isGenerated >> not)

                for file in files do
                    let lines = File.ReadAllLines file
                    let! _, checkResults = checker.GetBackgroundCheckResultsForFileInProject(file, project.Options)
                    let! unused = UnusedOpens.getUnusedOpens (checkResults, fun line -> lines[line - 1])

                    for range in unused do
                        found[(file, range)] <-
                            { File = file
                              Range = range
                              Line = lines[range.StartLine - 1].Trim() }

                log $"  looked for unused opens in {files.Length} files of {References.name project.File}: {clock.Elapsed.TotalSeconds:F1}s"

        if errors.Count > 0 then
            return Error(Seq.toList errors)
        else
            return Ok(found.Values |> Seq.sortBy (fun c -> c.File, c.Range.StartLine) |> Seq.toList)
    }

let find (checker: FSharpChecker) (projects: Project list) = findWith ignore checker projects

// ---- removing ----------------------------------------------------------------------------

type Outcome =
    {
      Removed: Candidate list
      /// Left in place, and why.
      Kept: (Candidate * string) list
      Files: string list
      /// Each changed file with its text before and after.
      Edited: (string * string * string) list
    }

/// An open alone on its line, with at most a comment after it: the only kind whose line can just go.
let private deletable =
    Regex(@"^\s*open(\s+type)?\s+[^\s/;]+\s*(//.*)?$", RegexOptions.Compiled)

let private hasConditional (lines: string[]) =
    lines |> Array.exists (fun line -> line.TrimStart().StartsWith "#if")

/// `text` without the given 1-based lines, and without a blank line the deletion left doubled.
let private edit (text: string) (deleted: Set<int>) =
    let lines = text.Split '\n'
    let kept = ResizeArray<string>()
    let mutable gap = false

    for i in 0 .. lines.Length - 1 do
        if deleted.Contains(i + 1) then
            gap <- true
        else
            let doubled =
                gap
                && kept.Count > 0
                && String.IsNullOrWhiteSpace lines[i]
                && String.IsNullOrWhiteSpace kept[kept.Count - 1]

            if not doubled then
                kept.Add lines[i]

            gap <- false

    String.Join("\n", kept)

/// What the process holds, and how much of it the runtime says is in use (garbage counts until collected).
let private rss () =
    $"{Environment.WorkingSet / 1048576L} MB held, {GC.GetTotalMemory false / 1048576L} MB in use"

let private nameOf (symbolUse: FSharpSymbolUse) =
    try
        symbolUse.Symbol.FullName
    with _ ->
        "?"

/// What the compiler service makes of one file with `text` as its contents, against the project as it
/// is: the file's errors (where, and what), and where it resolved every name, in source order with each
/// name's line.
/// The last is what a removal must not change. The project's earlier files come from what is already
/// checked, so this costs one file. Only these small arrays are kept; the check results go.
let private checkFile (checker: FSharpChecker) (project: Project) (file: string) (version: int) (text: string) =
    async {
        let! parsed, answer = checker.ParseAndCheckFileInProject(file, version, SourceText.ofString text, project.Options)

        match answer with
        | FSharpCheckFileAnswer.Aborted -> return Error "the compiler service gave up checking the file"
        | FSharpCheckFileAnswer.Succeeded results ->
            let errors =
                Array.append parsed.Diagnostics results.Diagnostics
                |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
                |> Array.map (fun d -> $"{d.FileName}({d.StartLine},{d.StartColumn})", d.Message)
                |> Array.toList

            let uses =
                results.GetAllUsesOfAllSymbolsInFile()
                |> Seq.map (fun u -> u.Range.StartLine, u.Range.StartColumn, u.Range.EndColumn, nameOf u)
                |> Seq.toArray
                |> Array.sort
                |> Array.map (fun (line, _, _, name) -> line, name)

            return Ok(errors, uses)
    }

/// Whether the names in `after` are the ones in `before` less what was on the deleted lines.
let private sameNames (before: (int * string)[]) (deleted: Set<int>) (after: (int * string)[]) =
    let expected = before |> Array.filter (fun (line, _) -> not (deleted.Contains line)) |> Array.map snd
    let actual = Array.map snd after

    if expected = actual then
        None
    else
        let at =
            Seq.zip expected actual
            |> Seq.tryFindIndex (fun (a, b) -> a <> b)
            |> Option.defaultValue (min expected.Length actual.Length)

        let say (names: string[]) = if at < names.Length then names[at] else "(nothing)"
        Some $"removing opens here changes what a name resolves to: {say expected} became {say actual}"

/// How many files are checked at once. Each check is small, but not free, and a machine that is
/// already short of memory gets one at a time.
let private parallelism () =
    let cores = max 1 (min 4 Environment.ProcessorCount)

    match Memory.availableBytes () with
    | Some free when free < 3L * 1024L * 1024L * 1024L -> 1
    | _ -> cores

/// Settles which of a file's opens can go, given a way to try a set of them: all at once, and if that
/// fails each half, down to single opens, which are set aside with the reason.
let rec private settleFile
    (attempt: Candidate list -> Async<string option>)
    (accepted: Candidate list)
    (batch: Candidate list)
    : Async<Candidate list * (Candidate * string) list> =
    async {
        match batch with
        | [] -> return accepted, []
        | _ ->
            match! attempt (accepted @ batch) with
            | None -> return accepted @ batch, []
            | Some why ->
                match batch with
                | [ single ] -> return accepted, [ single, $"removing it is rejected: {why}" ]
                | _ ->
                    let left, right = List.splitAt (batch.Length / 2) batch
                    let! accepted, rejectedLeft = settleFile attempt accepted left
                    let! accepted, rejectedRight = settleFile attempt accepted right
                    return accepted, rejectedLeft @ rejectedRight
    }

/// What is left to try in a file: the opens already shown safe, and batches still to be tried, the
/// first of which is being tried now.
type private FileState =
    { Accepted: Candidate list
      Queue: Candidate list list }

/// Removes the candidates whose removal is safe, from the files (or, in a preview, from memory).
/// With `realBuild`, once the removals type-check the projects are built with `dotnet build` too.
let fixWith
    (log: string -> unit)
    (checker: FSharpChecker)
    (realBuild: bool)
    (progress: string -> unit)
    (mode: Fix.Mode)
    (projects: Project list)
    (candidates: Candidate list)
    : Async<Outcome> =
    async {
        let originals =
            candidates
            |> List.map (fun c -> c.File)
            |> List.distinct
            |> List.map (fun file -> file, File.ReadAllBytes file)
            |> Map.ofList

        let lines =
            originals
            |> Map.map (fun _ bytes -> if Fix.isUtf16 bytes then None else Some((Fix.decode bytes).Split '\n'))

        let refusal (c: Candidate) =
            match lines[c.File] with
            | None -> Some "its file isn't UTF-8"
            | Some fileLines when hasConditional fileLines ->
                Some "its file has conditional compilation, and a use in a branch that isn't compiled can't be seen"
            | Some _ when c.Range.StartLine <> c.Range.EndLine -> Some "it spans several lines"
            | Some fileLines when not (deletable.IsMatch(fileLines[c.Range.StartLine - 1].TrimEnd '\r')) ->
                Some "it shares its line with other code"
            | Some _ -> None

        let skipped, editable =
            candidates
            |> List.map (fun c -> c, refusal c)
            |> List.partition (fun (_, why) -> why.IsSome)

        let skipped = skipped |> List.map (fun (c, why) -> c, why.Value)
        let editable = editable |> List.map fst

        // What each file should currently contain; in a preview, what it would.
        let current = Dictionary<string, byte[]>()
        originals |> Map.iter (fun file bytes -> current[file] <- bytes)
        let stamps = Dictionary<string, DateTime>()

        /// Makes the files what removing `chosen` gives, and every other file what it was.
        let apply (chosen: Candidate list) =
            let removed =
                chosen
                |> List.groupBy (fun c -> c.File)
                |> List.map (fun (file, cs) -> file, cs |> List.map (fun c -> c.Range.StartLine) |> Set.ofList)
                |> Map.ofList

            for KeyValue(file, original) in originals do
                let wanted =
                    match Map.tryFind file removed with
                    | Some deleted -> Fix.encode original (edit (Fix.decode original) deleted)
                    | None -> original

                if wanted <> current[file] then
                    if mode = Fix.Apply then
                        File.WriteAllBytes(file, wanted)

                    current[file] <- wanted
                    stamps[file] <- DateTime.UtcNow

            removed

        let short (error: string) =
            error.Replace(Environment.CurrentDirectory + "/", "")

        // Cancelling isn't an exception, so `with` wouldn't see it; `finally` does.
        let completed = ref false

        try
            let clock = Diagnostics.Stopwatch.StartNew()
            progress "type-checking the projects as they are..."
            let! errors = Analysis.typeErrors checker projects
            log $"  baseline: {clock.Elapsed.TotalSeconds:F1}s [{rss ()}]"

            if not errors.IsEmpty then
                failwithf "the projects don't type-check before any change: %s" (short errors.Head)

            // ---- each file on its own ----------------------------------------------------------
            // An open only affects its own file, so a file's opens are tried against that file alone,
            // in memory, several files at once. Nothing is written, and nothing but small arrays is
            // kept, so this costs a file's check per try rather than a whole solution's.
            let byFile = editable |> List.groupBy (fun c -> c.File)

            let projectsOf (file: string) =
                projects |> List.filter (fun project -> Analysis.sourceFiles project.Options |> Array.contains file)

            let checks = ref 0
            let versions = ref 0
            let filesDone = ref 0
            let workers = parallelism ()
            progress $"checking {editable.Length} opens, one file at a time ({byFile.Length} files, {workers} at once)..."
            clock.Restart()

            let verifyFile (file: string, opens: Candidate list) =
                async {
                    let original = Fix.decode originals[file]
                    let owners = projectsOf file

                    let run (project: Project) (text: string) =
                        Interlocked.Increment(&checks.contents) |> ignore
                        checkFile checker project file (Interlocked.Increment(&versions.contents)) text

                    let! baselines =
                        owners
                        |> List.map (fun project ->
                            async {
                                let! result = run project original
                                return project, result
                            })
                        |> Async.Sequential

                    // A file checked on its own can report errors the whole project doesn't (interpolated
                    // strings with typed holes, `%s{x}`, do), so what it says before any change is what it is
                    // expected to say after: the same errors, less their positions, and the same names.
                    let usable =
                        baselines
                        |> Array.choose (fun (project, result) ->
                            match result with
                            | Ok(errors, uses) -> Some(project, errors |> List.map snd |> List.sort, uses)
                            | _ -> None)

                    let outcome =
                        async {
                            if owners.IsEmpty || usable.Length <> baselines.Length then
                                let reason =
                                    baselines
                                    |> Array.tryPick (fun (_, result) ->
                                        match result with
                                        | Error why -> Some why
                                        | Ok _ -> None)
                                    |> Option.defaultValue "no project has it"

                                log $"  can't judge {Path.GetFileName file} alone: {reason}"
                                return None
                            else
                                // Every project the file is in must agree.
                                let attempt (chosen: Candidate list) =
                                    async {
                                        let deleted = chosen |> List.map (fun c -> c.Range.StartLine) |> Set.ofList
                                        let text = edit original deleted

                                        let rec go index =
                                            async {
                                                if index = usable.Length then
                                                    return None
                                                else
                                                    let project, expectedErrors, before = usable[index]

                                                    match! run project text with
                                                    | Error why -> return Some why
                                                    | Ok(errors, _) when List.sort (List.map snd errors) <> expectedErrors ->
                                                        let expected = Set.ofList expectedErrors

                                                        let fresh =
                                                            errors |> List.tryFind (fun (_, message) -> not (expected.Contains message))

                                                        match fresh with
                                                        | Some(where, message) -> return Some(short $"{where}: {message}")
                                                        | None -> return Some "an error it had is gone"
                                                    | Ok(_, after) ->
                                                        match sameNames before deleted after with
                                                        | Some why -> return Some why
                                                        | None -> return! go (index + 1)
                                            }

                                        return! go 0
                                    }

                                let! accepted, rejected = settleFile attempt [] (opens |> List.sortBy (fun c -> c.Range.StartLine))
                                return Some(accepted, rejected)
                        }

                    let! result = outcome
                    let finished = Interlocked.Increment(&filesDone.contents)

                    // The runtime is content to let garbage pile up while there is room; hand it back now and
                    // then, so what this phase holds doesn't depend on how many files there are.
                    if finished % 100 = 0 then
                        GC.Collect(2, GCCollectionMode.Aggressive, true, true)

                    if finished % 50 = 0 then
                        log $"  {finished}/{byFile.Length} files checked: {clock.Elapsed.TotalSeconds:F0}s [{rss ()}]"

                    return file, result
                }

            let! verified = Async.Parallel(List.map verifyFile byFile, maxDegreeOfParallelism = workers)

            let settled =
                verified |> Array.choose (fun (file, result) -> result |> Option.map (fun r -> file, r)) |> Map.ofArray

            let setAside = settled |> Map.toList |> List.collect (fun (_, (_, rejected)) -> rejected)
            let removable = settled |> Map.toList |> List.sumBy (fun (_, (accepted, _)) -> accepted.Length)
            let unjudged = byFile.Length - settled.Count
            GC.Collect(2, GCCollectionMode.Aggressive, true, true)

            let summary =
                $"  files on their own: {removable} can go, {setAside.Length} set aside, {unjudged} files left to the whole solution; {checks.Value} file checks: {clock.Elapsed.TotalSeconds:F1}s [{rss ()}]"

            log summary

            // ---- the whole solution -------------------------------------------------------------
            // What a file's own check can't see is a change in what it offers other files, so the accepted
            // opens are checked together, once. Only when that fails, or a file couldn't be judged alone, are
            // there more rounds.
            let check (chosen: Candidate list) =
                async {
                    let clock = Diagnostics.Stopwatch.StartNew()
                    apply chosen |> ignore
                    log $"    edited the files: {clock.Elapsed.TotalSeconds:F1}s"
                    clock.Restart()

                    // The results of an earlier check would sit beside the new ones until something evicted them.
                    checker.InvalidateAll()
                    GC.Collect(2, GCCollectionMode.Aggressive, true, true)

                    let! errors =
                        match mode with
                        | Fix.Apply -> Analysis.typeErrors checker projects
                        | Fix.Preview ->
                            async {
                                return
                                    lock Fix.previewLock (fun () ->
                                        let real = FileSystemAutoOpens.FileSystem

                                        FileSystemAutoOpens.FileSystem <-
                                            Fix.Overlay(Dictionary<string, byte[]>(current), Dictionary<string, DateTime>(stamps))

                                        try
                                            Analysis.typeErrors checker projects |> Async.RunSynchronously
                                        finally
                                            FileSystemAutoOpens.FileSystem <- real)
                            }

                    log $"    checked: {clock.Elapsed.TotalSeconds:F1}s [{rss ()}]"
                    clock.Restart()

                    // Every check leaves a whole compiler's worth of type-checked projects behind.
                    GC.Collect(2, GCCollectionMode.Aggressive, true, true)
                    log $"    collected garbage: {clock.Elapsed.TotalSeconds:F1}s [{rss ()}]"

                    if errors.IsEmpty && realBuild && mode = Fix.Apply then
                        progress "building the projects with dotnet build..."
                        return! Fix.buildErrors (projects |> List.map (fun project -> project.File) |> List.distinct)
                    else
                        return errors
                }

            let rec loop (state: Map<string, FileState>) (rejected: (Candidate * string) list) =
                async {
                    let head (s: FileState) =
                        match s.Queue with
                        | batch :: _ -> batch
                        | [] -> []

                    let tried = state |> Map.filter (fun _ s -> not s.Accepted.IsEmpty || not s.Queue.IsEmpty)
                    let chosen = tried |> Map.toList |> List.collect (fun (_, s) -> s.Accepted @ head s)
                    let pending = state |> Map.exists (fun _ s -> not s.Queue.IsEmpty)

                    if chosen.IsEmpty then
                        return state, rejected
                    else
                        let clock = Diagnostics.Stopwatch.StartNew()
                        progress $"checking {chosen.Length} opens in {tried.Count} files..."
                        let! errors = check chosen
                        let took = $"{clock.Elapsed.TotalSeconds:F0}s"

                        // A batch that went through joins what's accepted.
                        let advance (s: FileState) =
                            match s.Queue with
                            | batch :: rest -> { Accepted = s.Accepted @ batch; Queue = rest }
                            | [] -> s

                        if errors.IsEmpty then
                            progress $"  type-checks ({took})"
                            let state = state |> Map.map (fun _ s -> advance s)

                            // A clean round that finished every queue has just checked the final set.
                            if state |> Map.exists (fun _ s -> not s.Queue.IsEmpty) then
                                return! loop state rejected
                            else
                                return state, rejected
                        else
                            progress $"  {errors.Length} errors ({took})"
                            let blame (file: string) = errors |> List.tryFind (fun e -> e.StartsWith(file + "("))
                            let attributed = tried |> Map.exists (fun file _ -> (blame file).IsSome)

                            // An error in none of the files being edited: whatever is being tried is suspect.
                            let culprit file (s: FileState) =
                                match blame file with
                                | Some error -> Some error
                                | None when not attributed && (not s.Queue.IsEmpty || not pending) -> Some errors.Head
                                | None -> None

                            let reason error = $"removing it is rejected: {short error}"

                            let state, rejected =
                                tried
                                |> Map.fold
                                    (fun (state: Map<string, FileState>, rejected) file s ->
                                        match culprit file s with
                                        | None -> Map.add file (advance s) state, rejected
                                        | Some error ->
                                            match s.Queue with
                                            | [ single ] :: rest -> Map.add file { s with Queue = rest } state, (single, reason error) :: rejected
                                            | batch :: rest ->
                                                let left, right = List.splitAt (batch.Length / 2) batch
                                                Map.add file { s with Queue = left :: right :: rest } state, rejected
                                            | [] ->
                                                // What was accepted fails together.
                                                match s.Accepted with
                                                | [ single ] ->
                                                    Map.add file { Accepted = []; Queue = [] } state, (single, reason error) :: rejected
                                                | many -> Map.add file { Accepted = []; Queue = [ many ] } state, rejected)
                                    (state, rejected)

                            return! loop state rejected
                }

            let initial =
                byFile
                |> List.map (fun (file, opens) ->
                    match Map.tryFind file settled with
                    | Some(accepted, _) -> file, { Accepted = []; Queue = (if accepted.IsEmpty then [] else [ accepted ]) }
                    | None -> file, { Accepted = []; Queue = [ opens |> List.sortBy (fun c -> c.Range.StartLine) ] })
                |> Map.ofList

            let! state, rejected = loop initial setAside
            let accepted = state |> Map.toList |> List.collect (fun (_, s) -> s.Accepted)

            // Leave the files as the accepted set alone makes them, whatever was tried last.
            apply accepted |> ignore

            let edited =
                originals
                |> Map.toList
                |> List.filter (fun (file, original) -> current[file] <> original)
                |> List.map (fun (file, original) ->
                    // The byte order mark is part of the file's first line.
                    let mark = if Fix.hasBom original then "﻿" else ""
                    file, mark + Fix.decode original, mark + Fix.decode current[file])

            completed.Value <- true

            return
                { Removed = accepted |> List.sortBy (fun c -> c.File, c.Range.StartLine)
                  Kept = skipped @ (rejected |> List.sortBy (fun (c, _) -> c.File, c.Range.StartLine))
                  Files = edited |> List.map (fun (file, _, _) -> file)
                  Edited = edited }
        finally
            // Put every file back when the run fails or is stopped.
            if not completed.Value then
                apply [] |> ignore
    }

/// Like `fixWith`, without timings.
let fix
    (checker: FSharpChecker)
    (realBuild: bool)
    (progress: string -> unit)
    (mode: Fix.Mode)
    (projects: Project list)
    (candidates: Candidate list)
    : Async<Outcome> =
    fixWith ignore checker realBuild progress mode projects candidates
