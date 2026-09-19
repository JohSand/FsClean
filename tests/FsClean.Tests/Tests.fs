module FsClean.Tests

open System
open System.Diagnostics
open System.IO
open Expecto
open FSharp.Compiler.CodeAnalysis
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
          } ]

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
