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

let private analyze fixture wholeProgram =
    analyzeProjects [ $"{fixture}/{fixture}.fsproj" ] wholeProgram

let private deadSample = analyze "DeadCodeSample" false
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
         let result = Fix.run projects before.Dead |> Async.RunSynchronously

         { Directory = directory
           Before = before
           Result = result
           ErrorsAfter = Analysis.typeErrors (FSharpChecker.Create()) projects |> Async.RunSynchronously
           After = analyzeLoaded projects })

let private deadFixed = fixFixture "DeadCodeSample"
let private layoutFixed = fixFixture "RemovalSample"

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
                    "RemovalSample.Layout.deadHead" ] // a chain can't lose its first declaration
                  "left in place"
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
              let directory, project = copyFixture "DeadCodeSample"
              let projects = ProjectLoader.load [ project ]
              let report = analyzeLoaded projects

              // A wrong finding: usedFunction is called from main.
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

              let result = Fix.run projects (report.Dead @ [ wrong ]) |> Async.RunSynchronously

              Expect.equal (names result.Removed) (names report.Dead) "the real findings still go"

              Expect.isTrue
                  (result.Kept
                   |> List.exists (fun (decl, why) -> decl.Name = wrong.Name && why.Contains "doesn't type-check"))
                  "the wrong one is kept, with the compiler's reason"

              Expect.isTrue ((File.ReadAllText library).Contains "let usedFunction") "and it's still in the file"

              Expect.isEmpty
                  (Analysis.typeErrors (FSharpChecker.Create()) projects |> Async.RunSynchronously)
                  "the result type-checks"
          } ]

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
