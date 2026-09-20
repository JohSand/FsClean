module FsClean.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FsClean.Analysis

let private usage =
    """fsclean - find dead code, and unneeded project references, in F# projects

USAGE
    fsclean [options] <project.fsproj | solution.slnx>...
    fsclean opens [options] <project.fsproj | solution.slnx>...

OPTIONS
    --exclude <text>  Leave out the projects whose path contains <text> (repeatable). For the project of a
                      solution that doesn't build. A project that isn't left out and references one
                      that is still brings it in.
    --whole-program   Treat only entry points as roots, even in libraries. Use when every
                      consumer of a library is among the projects given.
    --explain <text>  Also explain each declaration whose name contains <text>: what uses it,
                      what it uses, and what keeps it alive.
    --references      List every project reference with the uses behind it, not only the ones
                      with no compile-time use.
    --fix             Remove the dead code from the source files. Each removal is checked: it only
                      stays if the projects still type-check. Commit first, so it can be reviewed
                      with git diff.
    --build           With --fix, also build the projects with dotnet build once the removals type-check,
                      and refuse whatever it rejects. Slower, and catches what the F# compiler service
                      used for the type-check accepts and the compiler does not.
    --timings         Report on stderr how long each phase took.
    --dry             Show what --fix would remove, and the diff, with the same checks, but change
                      no file. Implies --fix.
    -h, --help        Show this help.

COMMANDS
    opens             List the `open` declarations the compiler service considers unused. With --fix,
                      remove them: each removal is kept only if the projects still type-check and every
                      name in the edited files still resolves to the same symbol. --dry previews it,
                      --build also builds with dotnet build, --exclude and --timings work as above.
                      Files with #if are left alone. Commit first.

The projects must be restored first (dotnet restore)."""

type private Args =
    { Projects: string list
      Options: Options
      AllReferences: bool
      Fix: bool
      Dry: bool
      Build: bool
      Timings: bool
      Exclude: string list }

type private OpensArgs =
    { Projects: string list
      Fix: bool
      Dry: bool
      Build: bool
      Timings: bool
      Exclude: string list }

type private Command =
    | Help
    | Run of Args
    | Opens of OpensArgs
    | Invalid of string

let private parseArgs (argv: string list) =
    let rec go (args: Args) rest =
        match rest with
        | [] when List.isEmpty args.Projects -> Invalid "no project given"
        | [] -> Run { args with Projects = List.rev args.Projects }
        | ("-h" | "--help") :: _ -> Help
        | "--whole-program" :: rest -> go { args with Options = { args.Options with WholeProgram = true } } rest
        | "--explain" :: text :: rest -> go { args with Options = { args.Options with Explain = Some text } } rest
        | [ "--explain" ] -> Invalid "--explain needs a value"
        | "--references" :: rest -> go { args with AllReferences = true } rest
        | "--fix" :: rest -> go { args with Fix = true } rest
        | "--dry" :: rest -> go { args with Fix = true; Dry = true } rest
        | "--build" :: rest -> go { args with Build = true } rest
        | "--timings" :: rest -> go { args with Timings = true } rest
        | "--exclude" :: text :: rest -> go { args with Exclude = text :: args.Exclude } rest
        | [ "--exclude" ] -> Invalid "--exclude needs a value"
        | flag :: _ when flag.StartsWith "-" -> Invalid $"unknown option {flag}"
        | project :: rest -> go { args with Projects = project :: args.Projects } rest

    go
        { Projects = []
          Options = { WholeProgram = false; Explain = None }
          AllReferences = false
          Fix = false
          Dry = false
          Build = false
          Timings = false
          Exclude = [] }
        argv

let private parseOpensArgs (argv: string list) =
    let rec go (args: OpensArgs) rest =
        match rest with
        | [] when List.isEmpty args.Projects -> Invalid "no project given"
        | [] -> Opens { args with Projects = List.rev args.Projects }
        | ("-h" | "--help") :: _ -> Help
        | "--fix" :: rest -> go { args with Fix = true } rest
        | "--dry" :: rest -> go { args with Fix = true; Dry = true } rest
        | "--build" :: rest -> go { args with Build = true } rest
        | "--timings" :: rest -> go { args with Timings = true } rest
        | "--exclude" :: text :: rest -> go { args with Exclude = text :: args.Exclude } rest
        | [ "--exclude" ] -> Invalid "--exclude needs a value"
        | flag :: _ when flag.StartsWith "-" -> Invalid $"unknown option {flag} for opens"
        | project :: rest -> go { args with Projects = project :: args.Projects } rest

    go { Projects = []; Fix = false; Dry = false; Build = false; Timings = false; Exclude = [] } argv

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

let private printKept (kept: (Decl * string) list) =
    if not kept.IsEmpty then
        printfn "Left in place (%d)" kept.Length

        for decl, reason in kept |> List.sortBy (fun (decl, _) -> decl.Range.FileName, decl.Range.StartLine) do
            printfn "  %-40s %-12s %s" (locate decl) decl.Kind decl.Name
            printfn "      %s" reason

        printfn ""

let private run (args: Args) =
    let projectPaths = args.Projects

    for path in projectPaths do
        if not (File.Exists path) then
            failwithf "no such project: %s" path

    use watch = Memory.watch (eprintfn "%s")
    let clock = System.Diagnostics.Stopwatch.StartNew()
    let projects = ProjectLoader.loadExcluding args.Exclude projectPaths
    let log = if args.Timings then eprintfn "%s" else ignore
    log $"  loaded {projects.Length} projects: {clock.Elapsed.TotalSeconds:F1}s"
    let names =
        projects |> List.map (fun project -> References.name project.File) |> List.distinct |> List.sort

    eprintfn "Analyzing %d projects: %s" names.Length (String.concat ", " names)
    let checker = Analysis.createChecker projects

    match Analysis.analyzeWith log checker args.Options projects |> watch.Run with
    | Error errors ->
        let shown = 20
        eprintfn "The projects don't type-check, so the analysis would be unreliable (%d errors):" errors.Length

        for error in List.truncate shown errors do
            eprintfn "  %s" error

        if errors.Length > shown then
            eprintfn "  ... and %d more" (errors.Length - shown)

        let compiler = typeof<FSharpChecker>.Assembly.GetName().Version
        eprintfn ""
        eprintfn "If `dotnet build` accepts these projects, they may use newer F# language features than the"
        eprintfn "compiler service this fsclean was built with (FSharp.Compiler.Service %O)." compiler
        1
    | Ok report ->
        if not args.Fix then
            printSection "Dead code" report.Dead

        printSection "Unused, but the initializer may have side effects (kept; review by hand)" report.NeedsReview
        printReferences args.AllReferences report.References

        if args.Fix then
            let mode = if args.Dry then Fix.Preview else Fix.Apply
            eprintfn "Checking that the removals still type-check..."
            let run = if args.Build then Fix.runBuilt else Fix.runWith
            let result = run checker (eprintfn "%s") mode projects report.Dead |> watch.Run
            printSection (if args.Dry then "Would remove" else "Removed") result.Removed
            printKept result.Kept

            if args.Dry then
                for file, before, after in result.Edited do
                    let path = Path.GetRelativePath(Environment.CurrentDirectory, file)

                    for line in Diff.unified path before after do
                        printfn "%s" line

                printfn ""

            if args.Dry then
                printfn "Would remove %d declarations from %d files. Nothing was changed." result.Removed.Length result.Files.Length
            else
                printfn "Removed %d declarations from %d files." result.Removed.Length result.Files.Length

        for explanation in report.Explanations do
            printfn "%s\n" explanation

        if not args.Fix then
            printfn "%d declarations analyzed, %d dead." report.Analyzed report.Dead.Length

        0

let private runOpens (args: OpensArgs) =
    for path in args.Projects do
        if not (File.Exists path) then
            failwithf "no such project: %s" path

    use watch = Memory.watch (eprintfn "%s")
    let clock = System.Diagnostics.Stopwatch.StartNew()
    let projects = ProjectLoader.loadExcluding args.Exclude args.Projects
    let log = if args.Timings then eprintfn "%s" else ignore
    log $"  loaded {projects.Length} projects: {clock.Elapsed.TotalSeconds:F1}s"
    let checker = Analysis.createChecker projects

    match Opens.findWith log checker projects |> watch.Run with
    | Error errors ->
        eprintfn "The projects don't type-check, so the analysis would be unreliable (%d errors):" errors.Length

        for error in List.truncate 20 errors do
            eprintfn "  %s" error

        1
    | Ok candidates ->
        log $"  found {candidates.Length} candidates: {clock.Elapsed.TotalSeconds:F1}s"

        let show (candidate: Opens.Candidate) =
            let file = Path.GetRelativePath(Environment.CurrentDirectory, candidate.File)
            $"{file}:{candidate.Range.StartLine}  {candidate.Line}"

        if not args.Fix then
            for candidate in candidates do
                printfn "%s" (show candidate)

            printfn ""
            printfn "%d unused opens." candidates.Length
            0
        else
            let mode = if args.Dry then Fix.Preview else Fix.Apply
            eprintfn "Checking that removing them still type-checks..."

            let result =
                Opens.fixWith log checker args.Build (eprintfn "%s") mode projects candidates
                |> watch.Run

            if not result.Removed.IsEmpty then
                printfn "%s (%d)" (if args.Dry then "Would remove" else "Removed") result.Removed.Length

                for candidate in result.Removed do
                    printfn "  %s" (show candidate)

                printfn ""

            if not result.Kept.IsEmpty then
                printfn "Left in place (%d)" result.Kept.Length

                for candidate, why in result.Kept do
                    printfn "  %s" (show candidate)
                    printfn "      %s" why

                printfn ""

            if args.Dry then
                for file, before, after in result.Edited do
                    let path = Path.GetRelativePath(Environment.CurrentDirectory, file)

                    for line in Diff.unified path before after do
                        printfn "%s" line

                printfn ""
                printfn "Would remove %d opens from %d files. Nothing was changed." result.Removed.Length result.Files.Length
            else
                printfn "Removed %d opens from %d files." result.Removed.Length result.Files.Length

            0

/// The run was stopped for want of memory; any file a `--fix` had touched is back as it was.
let private outOfMemory () =
    eprintfn "fsclean: stopped because the machine was about to run out of memory. Nothing was changed."
    eprintfn "Close other programs, or analyze fewer projects at a time (--exclude), and try again."
    3

[<EntryPoint>]
let main argv =
    let command =
        match List.ofArray argv with
        | "opens" :: rest -> parseOpensArgs rest
        | args -> parseArgs args

    match command with
    | Help ->
        printfn "%s" usage
        0
    | Invalid message ->
        eprintfn "fsclean: %s\n\n%s" message usage
        2
    | Run args ->
        try
            run args
        with
        | Failure message ->
            eprintfn "fsclean: %s" message
            1
        | Memory.Exhausted _ -> outOfMemory ()
    | Opens args ->
        try
            runOpens args
        with
        | Failure message ->
            eprintfn "fsclean: %s" message
            1
        | Memory.Exhausted _ -> outOfMemory ()
