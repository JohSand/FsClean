module FsClean.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FsClean.Analysis

let private usage =
    """fsclean - find dead code, and unneeded project references, in F# projects

USAGE
    fsclean [options] <project.fsproj>...

OPTIONS
    --whole-program   Treat only entry points as roots, even in libraries. Use when every
                      consumer of a library is among the projects given.
    --explain <text>  Also explain each declaration whose name contains <text>: what uses it,
                      what it uses, and what keeps it alive.
    --references      List every project reference with the uses behind it, not only the ones
                      with no compile-time use.
    -h, --help        Show this help.

The projects must be restored first (dotnet restore)."""

type private Command =
    | Help
    | Analyze of projects: string list * Options * allReferences: bool
    | Invalid of string

let private parseArgs (argv: string list) =
    let rec go projects options allReferences args =
        match args with
        | [] when List.isEmpty projects -> Invalid "no project given"
        | [] -> Analyze(List.rev projects, options, allReferences)
        | ("-h" | "--help") :: _ -> Help
        | "--whole-program" :: rest -> go projects { options with WholeProgram = true } allReferences rest
        | "--explain" :: text :: rest -> go projects { options with Explain = Some text } allReferences rest
        | [ "--explain" ] -> Invalid "--explain needs a value"
        | "--references" :: rest -> go projects options true rest
        | flag :: _ when flag.StartsWith "-" -> Invalid $"unknown option {flag}"
        | project :: rest -> go (project :: projects) options allReferences rest

    go [] { WholeProgram = false; Explain = None } false argv

let private locate (decl: Decl) =
    let file = Path.GetRelativePath(Environment.CurrentDirectory, decl.Range.FileName)
    $"{file}:{decl.Range.StartLine}:{decl.Range.StartColumn + 1}"

let private printSection title (decls: Decl list) =
    if not decls.IsEmpty then
        printfn "%s (%d)" title decls.Length

        for decl in decls do
            printfn "  %-40s %-12s %s" (locate decl) decl.Kind decl.Name

        printfn ""

let private names (projects: string list) =
    projects |> List.map References.name |> String.concat ", "

let private verdictText (finding: References.Finding) =
    match finding.Now, finding.AfterCleanup with
    | References.Unneeded, _ -> "no compile-time use: nothing declared in it is used"
    | References.OnlyTransitive provides, _ ->
        $"not used itself; it only carries {names provides}, which {References.name finding.Project} doesn't reference directly"
    | References.Needed, References.Unneeded -> "used only by dead code: unneeded once that's removed"
    | References.Needed, References.OnlyTransitive provides ->
        $"used only by dead code; once that's removed it only carries {names provides}"
    | References.Needed, _ -> "needed"
    | References.NotAnalyzed, _ -> "not analyzed: not an F# project that was loaded"

/// Needed references aren't worth a line; neither are ones there's no way to judge.
let private isFinding (finding: References.Finding) =
    match finding.Now, finding.AfterCleanup with
    | References.NotAnalyzed, _
    | References.Needed, References.Needed -> false
    | _ -> true

let private printReferences allReferences (findings: References.Finding list) =
    let shown =
        findings
        |> List.filter (fun finding -> allReferences || isFinding finding)
        |> List.sortBy (fun finding -> References.name finding.Project, References.name finding.Reference)

    if not shown.IsEmpty then
        printfn "%s (%d)" (if allReferences then "Project references" else "Project references worth a look") shown.Length

        for finding in shown do
            let pair = $"{References.name finding.Project} -> {References.name finding.Reference}"
            printfn "  %-44s %s" pair (verdictText finding)

            for example in finding.Evidence do
                printfn "      %s" example

        if not allReferences then
            printfn "  No compile-time use isn't the same as safe to remove: a reference can be needed at"
            printfn "  runtime (a plugin loaded by reflection, a project referenced to copy its output)."

        printfn ""

let private run (projectPaths: string list) options allReferences =
    for path in projectPaths do
        if not (File.Exists path) then
            failwithf "no such project: %s" path

    let projects = ProjectLoader.load projectPaths
    let checker = FSharpChecker.Create()

    match Analysis.analyze checker options projects |> Async.RunSynchronously with
    | Error errors ->
        eprintfn "The project doesn't type-check, so the analysis would be unreliable:"

        for error in errors do
            eprintfn "  %s" error

        1
    | Ok report ->
        printSection "Dead code" report.Dead
        printSection "Unused, but the initializer may have side effects (kept; review by hand)" report.NeedsReview
        printReferences allReferences report.References

        for explanation in report.Explanations do
            printfn "%s\n" explanation

        printfn "%d declarations analyzed, %d dead." report.Analyzed report.Dead.Length
        0

[<EntryPoint>]
let main argv =
    match parseArgs (List.ofArray argv) with
    | Help ->
        printfn "%s" usage
        0
    | Invalid message ->
        eprintfn "fsclean: %s\n\n%s" message usage
        2
    | Analyze(projects, options, allReferences) -> run projects options allReferences
