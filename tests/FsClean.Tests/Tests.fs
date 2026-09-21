module FsClean.Tests

open System
open System.Diagnostics
open System.IO
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FsClean

let private repoRoot =
    let rec up (dir: DirectoryInfo) =
        if File.Exists(Path.Combine(dir.FullName, "global.json")) then
            dir.FullName
        else
            up dir.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

/// The fixtures are real projects, and loading one needs it restored.
let private restore (project: string) =
    let info = ProcessStartInfo("dotnet", $"restore \"{project}\"")
    info.RedirectStandardOutput <- true
    info.RedirectStandardError <- true
    use restore = Process.Start info
    let output = restore.StandardOutput.ReadToEnd() + restore.StandardError.ReadToEnd()
    restore.WaitForExit()

    if restore.ExitCode <> 0 then
        failwithf "dotnet restore %s failed:\n%s" project output

/// Projects are paths below tests/fixtures.
let private analyzeProjects (fixtureProjects: string list) wholeProgram =
    lazy
        (let projects =
            fixtureProjects
            |> List.map (fun project -> Path.Combine(repoRoot, "tests", "fixtures", project))

         List.iter restore projects
         let options = { Analysis.WholeProgram = wholeProgram; Analysis.Explain = None }

         match
             Analysis.analyze (FSharpChecker.Create()) options (ProjectLoader.load projects)
             |> Async.RunSynchronously
         with
         | Ok report -> report
         | Error errors -> failwithf "%A don't type-check:\n%s" fixtureProjects (String.concat "\n" errors))

/// The projects loaded for a solution (or anything else) below tests/fixtures, by name.
let private loadedFrom (exclude: string list) (path: string) =
    let full = Path.Combine(repoRoot, "tests", "fixtures", path)
    restore full

    ProjectLoader.loadExcluding exclude [ full ]
    |> List.map (fun project -> References.name project.File)
    |> List.distinct
    |> List.sort

let private analyze fixture wholeProgram =
    analyzeProjects [ $"{fixture}/{fixture}.fsproj" ] wholeProgram

let private deadSample = analyze "DeadCodeSample" false
let private reflectionSample = analyze "ReflectionSample" false
let private libraryDefault = analyze "LibrarySample" false
let private libraryWholeProgram = analyze "LibrarySample" true

let private referenceApps = [ "References/RefApp/RefApp.fsproj"; "References/RefApp2/RefApp2.fsproj" ]
let private referencesDefault = analyzeProjects referenceApps false
let private referencesWholeProgram = analyzeProjects referenceApps true

let private copies = ResizeArray<string>()

AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
    for directory in copies do
        try
            Directory.Delete(directory, true)
        with _ ->
            ())

/// A private, restored copy of a fixture, so a test can let the tool rewrite it.
let private copyFixture (fixture: string) =
    let directory = Path.Combine(Path.GetTempPath(), "fsclean-tests", Guid.NewGuid().ToString "N")
    let source = Path.Combine(repoRoot, "tests", "fixtures", fixture)

    for file in Directory.EnumerateFiles(source, "*", SearchOption.AllDirectories) do
        let relative = Path.GetRelativePath(source, file)

        if not (relative.Split Path.DirectorySeparatorChar |> Array.exists (fun part -> part = "bin" || part = "obj")) then
            let target = Path.Combine(directory, relative)
            Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
            File.Copy(file, target)

    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(directory, "global.json"))
    copies.Add directory
    let project = Path.Combine(directory, fixture + ".fsproj")
    restore project
    directory, project

let private analyzeLoaded projects =
    let options = { Analysis.WholeProgram = false; Analysis.Explain = None }

    match Analysis.analyze (FSharpChecker.Create()) options projects |> Async.RunSynchronously with
    | Ok report -> report
    | Error errors -> failwithf "doesn't type-check:\n%s" (String.concat "\n" errors)

type private Fixed =
    { Directory: string
      Before: Analysis.Report
      Result: Fix.Result
      ErrorsAfter: string list
      After: Analysis.Report }

/// Runs the fix on a private copy of a fixture, then looks at what's left.
let private fixFixture fixture =
    lazy
        (let directory, project = copyFixture fixture
         let projects = ProjectLoader.load [ project ]
         let before = analyzeLoaded projects
         let result = Fix.run Fix.Apply projects before.Dead |> Async.RunSynchronously

         { Directory = directory
           Before = before
           Result = result
           ErrorsAfter = Analysis.typeErrors (FSharpChecker.Create()) projects |> Async.RunSynchronously
           After = analyzeLoaded projects })

let private deadFixed = fixFixture "DeadCodeSample"
let private layoutFixed = fixFixture "RemovalSample"

type private OpensFixed =
    { Directory: string
      Projects: ProjectLoader.Project list
      Found: Opens.Candidate list
      Outcome: Opens.Outcome }

/// Removes the unused opens from a private copy of a fixture.
let private opensFixed mode =
    let directory, project = copyFixture "OpensSample"
    let projects = ProjectLoader.load [ project ]
    let checker = Analysis.createChecker projects

    match Opens.find checker projects |> Async.RunSynchronously with
    | Error errors -> failwithf "doesn't type-check:\n%s" (String.concat "\n" errors)
    | Ok found ->
        { Directory = directory
          Projects = projects
          Found = found
          Outcome = Opens.fix checker false ignore mode projects found |> Async.RunSynchronously }

let private opensApplied = lazy (opensFixed Fix.Apply)
let private opensPreviewed = lazy (opensFixed Fix.Preview)

let private openLines (applied: OpensFixed) =
    applied.Found
    |> List.map (fun c -> $"{Path.GetFileName c.File}:{c.Range.StartLine} {c.Line}")


let private names (decls: Analysis.Decl list) =
    decls |> List.map (fun decl -> decl.Name) |> List.sort

let private verdict (verdict: References.Verdict) =
    match verdict with
    | References.Needed -> "needed"
    | References.Unneeded -> "unneeded"
    | References.OnlyTransitive provides -> "only carries " + String.concat ", " (List.map References.name provides)
    | References.NotAnalyzed -> "not analyzed"

/// "RefApp -> RefUtil: needed now, unneeded after cleanup", so a failure reads as what went wrong.
let private referenceLines (report: Analysis.Report) =
    report.References
    |> List.map (fun finding ->
        let now = verdict finding.Now
        let after = verdict finding.AfterCleanup
        let judged = if now = after then now else $"{now} now, {after} after cleanup"
        $"{References.name finding.Project} -> {References.name finding.Reference}: {judged}")
    |> List.sort

type private WrongFinding =
    { Directory: string
      Projects: ProjectLoader.Project list
      Report: Analysis.Report
      /// A live declaration reported dead, as if the analysis had made a mistake.
      Wrong: Analysis.Decl
      Result: Fix.Result
      /// What the run said it was doing.
      Progress: string list
      Library: string }

/// Asks for the real findings plus one wrong one: usedFunction is called from main.
let private wrongFinding mode =
    let directory, project = copyFixture "DeadCodeSample"
    let projects = ProjectLoader.load [ project ]
    let report = analyzeLoaded projects

    let library = Path.Combine(directory, "Library.fs")
    let lines = File.ReadAllLines library
    let line = 1 + Array.findIndex (fun (text: string) -> text.StartsWith "let usedFunction") lines
    let range = Range.mkRange library (Position.mkPos line 0) (Position.mkPos line lines[line - 1].Length)

    let wrong: Analysis.Decl =
        { Name = "DeadCodeSample.Library.usedFunction"
          Kind = "function"
          Range = range
          Extent =
            { Kind = Ast.LetBinding
              Range = range
              Chain = None
              Container = None
              IsPure = true }
          Component = 9999
          Blocker = None }

    let progress = ResizeArray<string>()

    { Directory = directory
      Projects = projects
      Report = report
      Wrong = wrong
      Result = Fix.runWith (Analysis.createChecker projects) progress.Add mode projects (report.Dead @ [ wrong ]) |> Async.RunSynchronously
      Progress = Seq.toList progress
      Library = library }

[<Tests>]
let tests =
    testList
        "FsClean"
        [ test "reports exactly the dead declarations of an executable" {
              let expected =
                  [ "unusedFunction" // never referenced
                    "onlyUsedByDead" // only referenced by dead code
                    "unusedCaller"
                    "pingUnused" // a dead cycle
                    "pongUnused"
                    "obsoleteFunction" // a neutral attribute doesn't make it a root
                    "UnusedRecord"
                    "UnusedRecord" // the `type UnusedRecord with` block that goes with it
                    "UnusedUnion"
                    "UnusedError"
                    "UnusedClass" // and so are its members, which aren't listed separately
                    "Calculator.NeverCalled" // a dead member of a live type
                    "Nested.unusedNested"
                    "TraceBuilder.NotBuilderRelated"
                    "Money.NeverUsed"
                    "Extensions.UnusedShout" ]
                  |> List.map (fun name -> "DeadCodeSample.Library." + name)
                  |> List.sort

              Expect.equal (names deadSample.Value.Dead) expected "dead declarations"
          }

          test "keeps what the compiler calls without reporting a use" {
              let dead = names deadSample.Value.Dead |> Set.ofList

              // Overrides and interface implementations, `and!`/`return!` builder members, and
              // extension blocks of types from inside and from outside the analyzed code.
              for live in
                  [ "Calculator.ToString"
                    "Calculator.Dispose"
                    "TraceBuilder.Bind2"
                    "TraceBuilder.ReturnFromFinal"
                    "BuilderExtensions.For"
                    "Money.Zero" // asked for by `List.sum`, through a member constraint
                    "Money.(+)"
                    "Extensions.GetEnumerator" ] do
                  Expect.isFalse (dead.Contains("DeadCodeSample.Library." + live)) $"{live} is alive"

              // Found by reflection, by convention: ASP.NET's middleware `Invoke`, and FsCheck's
              // static members returning `Arbitrary<_>`.
              for live in [ "Conventions.Middleware.Invoke"; "Conventions.Generators.GenInt" ] do
                  Expect.isFalse (dead.Contains("DeadCodeSample." + live)) $"{live} is alive"
          }

          test "lists an unused initializer with side effects for review, not as dead" {
              Expect.equal
                  (names deadSample.Value.NeedsReview)
                  [ "DeadCodeSample.Library.announcedAtStartup" ]
                  "needs review"
          }

          test "treats a library's public API as a root" {
              Expect.equal (names libraryDefault.Value.Dead) [ "LibrarySample.Api.neverCalled" ] "only the private dead one"
          }

          test "judges each project reference by what its project uses of it" {
              Expect.equal
                  (referenceLines referencesDefault.Value)
                  [ "RefApp -> RefCore: needed" // used directly
                    "RefApp -> RefMiddle: unneeded" // nothing of it is used, and RefCore is already direct
                    "RefApp -> RefUtil: needed now, unneeded after cleanup" // only dead code calls it
                    "RefApp2 -> RefMiddle: only carries RefCore" // the route to a project it doesn't reference
                    "RefMiddle -> RefCore: needed" ]
                  "reference verdicts"
          }

          test "counts a library's uncalled public API as dead when judging its references" {
              // Nothing in view calls RefMiddle.middleFn, and it's the only thing using RefCore.
              Expect.contains
                  (referenceLines referencesWholeProgram.Value)
                  "RefMiddle -> RefCore: needed now, unneeded after cleanup"
                  "RefMiddle's reference to RefCore"
          }

          test "shows the uses that justify a reference" {
              let evidence =
                  referencesDefault.Value.References
                  |> List.find (fun finding -> References.name finding.Project = "RefApp" && References.name finding.Reference = "RefCore")
                  |> fun finding -> finding.Evidence

              Expect.isTrue
                  (evidence |> List.exists (fun example -> example.StartsWith "RefCore.Core.used"))
                  $"evidence names the function that's used: %A{evidence}"
          }

          test "whole-program mode roots only entry points" {
              Expect.equal
                  (names libraryWholeProgram.Value.Dead)
                  [ "LibrarySample.Api.neverCalled"
                    "LibrarySample.Api.publicUnused"
                    "LibrarySample.Api.publicUsed"
                    "LibrarySample.Api.usedByPublic" ]
                  "no entry point, so nothing is a root"
          }

          test "removes every dead declaration and leaves code that still type-checks" {
              let fixedUp = deadFixed.Value
              Expect.equal (names fixedUp.Result.Removed) (names fixedUp.Before.Dead) "everything reported dead is removed"
              Expect.isEmpty fixedUp.Result.Kept "nothing is left in place"
              Expect.isEmpty fixedUp.ErrorsAfter "the projects still type-check"
              Expect.isEmpty fixedUp.After.Dead "and analysing the result finds nothing more"
          }

          test "takes the comments and attributes above a declaration with it, but not section dividers" {
              let text = File.ReadAllText(Path.Combine(deadFixed.Value.Directory, "Library.fs"))
              Expect.isFalse (text.Contains "Obsolete") "the attribute went with obsoleteFunction"
              Expect.isFalse (text.Contains "Dead: never referenced") "and so did the comment above unusedFunction"
              Expect.isTrue (text.Contains "// ---- functions ---") "a section divider stays"
              Expect.isTrue (text.Contains "// Live whenever Calculator is") "as does a comment on live code"
          }

          test "removes whole lines, and a trailing comment goes with its declaration" {
              let text = File.ReadAllText(Path.Combine(layoutFixed.Value.Directory, "Layout.fs"))

              for gone in [ "documentedDead"; "Documented, commented"; "nope"; "deadWithTrailing"; "goes with its line" ] do
                  Expect.isFalse (text.Contains gone) $"{gone} is gone"

              Expect.isTrue (text.Contains "let live1 x = x * 2 // a trailing comment on a live line stays") "a live line keeps its comment"
              Expect.isFalse (text.Contains "\n\n\n") "no run of blank lines is left behind"
          }

          test "removes from a chain what it can, and leaves what it can't" {
              let text = File.ReadAllText(Path.Combine(layoutFixed.Value.Directory, "Layout.fs"))
              Expect.isFalse (text.Contains "deadTail") "the last member of a `let rec ... and` chain goes"
              Expect.isTrue (text.Contains "and oddLive") "the rest of that chain stays"
              Expect.isFalse (text.Contains "deadA") "a chain that is all dead goes as one"
              Expect.isFalse (text.Contains "DeadLeaf") "and so does a dead `type ... and`"
              Expect.isTrue (text.Contains "LiveNode") "beside the live one"

              let kept = layoutFixed.Value.Result.Kept |> List.map (fun (decl, _) -> decl.Name) |> List.sort

              Expect.equal
                  kept
                  [ "RemovalSample.Layout.Holder.OnlyDead" // the type would be left with no members
                    "RemovalSample.Layout.deadHead" // a chain can't lose its first declaration
                    "RemovalSample.Vanishing.Gone.x" // its namespace would be left empty
                    "RemovalSample.Vanishing.Gone.y" ]
                  "left in place"
          }

          test "never leaves a namespace empty, since opening it would then fail" {
              let text = File.ReadAllText(Path.Combine(layoutFixed.Value.Directory, "Vanishing.fs"))
              Expect.isTrue (text.Contains "module Gone" && text.Contains "let x = 1") "the dead declarations stay"

              let why =
                  layoutFixed.Value.Result.Kept
                  |> List.filter (fun (decl, _) -> decl.Name.StartsWith "RemovalSample.Vanishing")
                  |> List.map snd
                  |> List.distinct

              Expect.isTrue (why |> List.forall (fun reason -> reason.Contains "namespace")) "and the reason says why"
          }

          test "removes a module once nothing is left in it, and the module around it too" {
              let text = File.ReadAllText(Path.Combine(layoutFixed.Value.Directory, "Layout.fs"))
              Expect.isFalse (text.Contains "AllDead") "a module of only dead declarations goes"
              Expect.isFalse (text.Contains "InnerDead") "so does a dead module inside a live one"
              Expect.isTrue (text.Contains "module Outer =" && text.Contains "let liveOuter") "which keeps what's live"
              Expect.isFalse (text.Contains "OuterDead") "emptying a module's last module empties that one as well"
          }

          test "removes an extension block with no live member, and the helper only it used" {
              let text = File.ReadAllText(Path.Combine(layoutFixed.Value.Directory, "Layout.fs"))
              Expect.isFalse (text.Contains "Shout1" || text.Contains "helper") "the block and its helper are gone"
              Expect.isFalse (text.Contains "module Ext") "and so is the module that held only them"
          }

          test "keeps a file's line endings and byte order mark" {
              let bytes = File.ReadAllBytes(Path.Combine(layoutFixed.Value.Directory, "Crlf.fs"))
              let text = Text.Encoding.UTF8.GetString bytes

              Expect.equal (Array.take 3 bytes) [| 0xEFuy; 0xBBuy; 0xBFuy |] "byte order mark"
              Expect.isFalse (text.Contains "deadCrlf") "the dead function is gone"
              Expect.isFalse (text.Replace("\r\n", "").Contains "\n") "every line ending is still CRLF"
          }

          test "keeps a declaration whose removal doesn't type-check, and everything else goes" {
              let scenario = wrongFinding Fix.Apply
              Expect.equal (names scenario.Result.Removed) (names scenario.Report.Dead) "the real findings still go"

              Expect.isTrue
                  (scenario.Result.Kept
                   |> List.exists (fun (decl, why) -> decl.Name = scenario.Wrong.Name && why.Contains "doesn't type-check"))
                  "the wrong one is kept, with the compiler's reason"

              Expect.isTrue ((File.ReadAllText scenario.Library).Contains "let usedFunction") "and it's still in the file"

              Expect.isEmpty
                  (Analysis.typeErrors (FSharpChecker.Create()) scenario.Projects |> Async.RunSynchronously)
                  "the result type-checks"
          }

          test "sets aside the removals an error names, instead of bisecting" {
              // The compiler says 'usedFunction' is not defined, which is the wrong finding.
              let checks = (wrongFinding Fix.Apply).Progress |> List.filter (fun line -> line.StartsWith "checking")
              Expect.equal checks.Length 2 "one check that fails, then one with the named removal set aside"
          }

          test "with a real build too, the removals are compiled with dotnet build and pass" {
              let _, project = copyFixture "DeadCodeSample"
              let projects = ProjectLoader.load [ project ]
              let report = analyzeLoaded projects
              let progress = ResizeArray<string>()
              let result = Fix.runBuilt (Analysis.createChecker projects) progress.Add Fix.Apply projects report.Dead |> Async.RunSynchronously

              Expect.equal (names result.Removed) (names report.Dead) "everything reported dead goes"
              Expect.isTrue (progress |> Seq.exists (fun line -> line.Trim() = "builds")) $"the build ran and passed: %A{Seq.toList progress}"
          }

          test "a solution stands for the F# projects it lists" {
              let all = [ "RefApp"; "RefApp2"; "RefCore"; "RefMiddle"; "RefUtil" ]
              Expect.equal (loadedFrom [] "References/References.slnx") all ".slnx, folders and other entries ignored"
              Expect.equal (loadedFrom [] "References/LegacyReferences.sln") all "a classic .sln"
          }

          test "a solution filter lists a subset, and what it references comes along" {
              // RefApp references three of the others; RefApp2 is left out.
              Expect.equal
                  (loadedFrom [] "References/References.slnf")
                  [ "RefApp"; "RefCore"; "RefMiddle"; "RefUtil" ]
                  "the filter's project and its references"
          }

          test "--exclude leaves out the projects a path matches" {
              Expect.equal
                  (loadedFrom [ "RefApp2" ] "References/References.slnx")
                  [ "RefApp"; "RefCore"; "RefMiddle"; "RefUtil" ]
                  "RefApp2 is excluded"
          }

          test "analyzing a solution finds what analyzing its projects does" {
              let solution = (analyzeProjects [ "References/References.slnx" ] false).Value
              Expect.equal (referenceLines solution) (referenceLines referencesDefault.Value) "same reference findings"
              Expect.equal (names solution.Dead) (names referencesDefault.Value.Dead) "same dead code"
          }

          test "a preview judges removals with the compiler too, without writing anything" {
              let scenario = wrongFinding Fix.Preview

              // Only possible if the compiler was shown the edited text: the wrong finding breaks main.
              Expect.equal (names scenario.Result.Removed) (names scenario.Report.Dead) "the real findings would go"

              Expect.isTrue
                  (scenario.Result.Kept
                   |> List.exists (fun (decl, why) -> decl.Name = scenario.Wrong.Name && why.Contains "doesn't type-check"))
                  "and the wrong one is still refused"

              for file in Directory.EnumerateFiles(scenario.Directory, "*.fs") do
                  let original = File.ReadAllBytes(Path.Combine(repoRoot, "tests", "fixtures", "DeadCodeSample", Path.GetFileName file))
                  Expect.equal (File.ReadAllBytes file) original $"{Path.GetFileName file} is untouched"

              let _, before, after = scenario.Result.Edited |> List.find (fun (file, _, _) -> file.EndsWith "Library.fs")
              Expect.isTrue (before.Contains "unusedFunction" && not (after.Contains "unusedFunction")) "the edit is reported"
              Expect.isTrue (after.Contains "let usedFunction") "without the wrong finding"
          }

          test "finds the opens nothing in a file resolves through" {
              Expect.equal
                  (openLines opensApplied.Value)
                  [ "Conditional.fs:3 open System.IO"
                    "Usage.fs:4 open System.IO"
                    "Usage.fs:5 open System.Collections.Generic"
                    "Usage.fs:7 open Sample.BuilderExtensions" ]
                  "the used opens aren't reported; the extension that the compiler service can't see is"
          }

          test "removes an unused open, and leaves the file as if it had never been there" {
              let applied = opensApplied.Value
              let usage = File.ReadAllText(Path.Combine(applied.Directory, "Usage.fs"))

              Expect.equal
                  (usage.Replace("\r\n", "\n"))
                  (File.ReadAllText(Path.Combine(repoRoot, "tests", "fixtures", "OpensSample", "Usage.fs"))
                      .Replace("\r\n", "\n")
                      .Replace("open System.IO\n", "")
                      .Replace("open System.Collections.Generic\n", ""))
                  "only those two lines are gone"
          }

          test "keeps an open the compiler needs, with the compiler's reason" {
              let applied = opensApplied.Value

              let kept =
                  applied.Outcome.Kept
                  |> List.find (fun (c, _) -> c.Line = "open Sample.BuilderExtensions")

              Expect.stringContains (snd kept) "'For' method" "why it stays"

              Expect.isTrue
                  ((File.ReadAllText(Path.Combine(applied.Directory, "Usage.fs"))).Contains "open Sample.BuilderExtensions")
                  "it's still in the file"

              Expect.isEmpty
                  (Analysis.typeErrors (FSharpChecker.Create()) applied.Projects |> Async.RunSynchronously)
                  "the result type-checks"
          }

          test "leaves a file with conditional compilation alone" {
              let applied = opensApplied.Value

              Expect.isTrue
                  (applied.Outcome.Kept
                   |> List.exists (fun (c, why) -> c.File.EndsWith "Conditional.fs" && why.Contains "conditional compilation"))
                  "reported, with the reason"

              Expect.isTrue
                  ((File.ReadAllText(Path.Combine(applied.Directory, "Conditional.fs"))).Contains "open System.IO")
                  "and its open is still there"
          }

          test "a preview of removing opens changes no file, and reports the edit" {
              let applied = opensPreviewed.Value

              Expect.equal
                  (applied.Outcome.Removed |> List.map (fun c -> c.Line))
                  [ "open System.IO"; "open System.Collections.Generic" ]
                  "the same removals as when applied"

              for file in Directory.EnumerateFiles(applied.Directory, "*.fs") do
                  let original = File.ReadAllText(Path.Combine(repoRoot, "tests", "fixtures", "OpensSample", Path.GetFileName file))
                  Expect.equal (File.ReadAllText file) original $"{Path.GetFileName file} is untouched"

              Expect.equal (applied.Outcome.Edited |> List.map (fun (file, _, _) -> Path.GetFileName file)) [ "Usage.fs" ] "one file edited in memory"
          }

          test "a fix stopped part-way, as when memory runs out, puts every file back" {
              let directory, project = copyFixture "DeadCodeSample"
              let projects = ProjectLoader.load [ project ]
              let dead = (analyzeLoaded projects).Dead
              let read () = [ for file in Directory.EnumerateFiles(directory, "*.fs") -> file, File.ReadAllText file ]
              let original = read ()
              use stop = new System.Threading.CancellationTokenSource()
              let mutable editedWhenStopped = false

              // The first check has run once a result is reported; the files hold the removals being tried.
              let progress (message: string) =
                  if message.StartsWith "  " && not stop.IsCancellationRequested then
                      editedWhenStopped <- read () <> original
                      stop.Cancel()

              Expect.throws
                  (fun () -> Fix.runWith (FSharpChecker.Create()) progress Fix.Apply projects dead |> fun work -> Async.RunSynchronously(work, cancellationToken = stop.Token) |> ignore)
                  "the run is cancelled"

              Expect.isTrue editedWhenStopped "there was something to put back"
              Expect.equal (read ()) original "every file is as it was"
          }

          test "removing opens, stopped part-way, puts every file back" {
              let directory, project = copyFixture "OpensSample"
              let projects = ProjectLoader.load [ project ]
              let checker = Analysis.createChecker projects
              let read () = [ for file in Directory.EnumerateFiles(directory, "*.fs") -> file, File.ReadAllText file ]
              let original = read ()
              use stop = new System.Threading.CancellationTokenSource()
              let mutable editedWhenStopped = false

              let found =
                  match Opens.find checker projects |> Async.RunSynchronously with
                  | Ok found -> found
                  | Error errors -> failwithf "doesn't type-check:\n%s" (String.concat "\n" errors)

              let progress (message: string) =
                  if message.StartsWith "  " && not stop.IsCancellationRequested then
                      editedWhenStopped <- read () <> original
                      stop.Cancel()

              Expect.throws
                  (fun () -> Opens.fix checker false progress Fix.Apply projects found |> fun work -> Async.RunSynchronously(work, cancellationToken = stop.Token) |> ignore)
                  "the run is cancelled"

              Expect.isTrue editedWhenStopped "there was something to put back"
              Expect.equal (read ()) original "every file is as it was"
          }

          test "DynamicallyAccessedMembers keeps the members of a type that reflection reaches" {
              // `All` keeps everything; the others only what their flags name; an attribute of any other
              // kind keeps the type it's on and nothing inside it; and an unmarked type is dead.
              Expect.equal
                  (names reflectionSample.Value.Dead)
                  [ "Sample.Reflected.AnyMethods.UnreachedProperty"
                    "Sample.Reflected.MarkedOnly.NotCoveredByTheMarker"
                    "Sample.Reflected.OnlyProperties.UnreachedMethod"
                    "Sample.Reflected.OnlyProperties.UnreachedPrivateProperty"
                    "Sample.Reflected.Plain" ]
                  "dead declarations"
          }

          test "a diff shows the deleted lines with their context" {
              Expect.equal
                  (Diff.unified "f.fs" "a\nb\nc\nd\ne\nf\ng\nh\n" "a\nb\nd\ne\nf\ng\nh\n")
                  [ "--- a/f.fs"; "+++ b/f.fs"; "@@ -1,6 +1,5 @@"; " a"; " b"; "-c"; " d"; " e"; " f" ]
                  "one hunk"

              Expect.isEmpty (Diff.unified "f.fs" "a\nb\n" "a\nb\n") "nothing to show when nothing changed"

              let text = [ for i in 1..20 -> string i ] |> String.concat "\n"
              let removed = [ for i in 1..20 do if i <> 2 && i <> 18 then string i ] |> String.concat "\n"
              let hunks = Diff.unified "f.fs" text removed |> List.filter (fun line -> line.StartsWith "@@")
              Expect.equal hunks.Length 2 "deletions far apart get a hunk each"
          }

          test "a diff keeps Windows line endings, so it applies to such a file" {
              Expect.equal
                  (Diff.unified "f.fs" "a\r\nb\r\n" "a\r\n")
                  [ "--- a/f.fs"; "+++ b/f.fs"; "@@ -1,2 +1,1 @@"; " a\r"; "-b\r" ]
                  "carriage returns stay on the lines"
          } ]

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
