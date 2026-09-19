/// Turns `.fsproj` files into the options FSharp.Compiler.Service needs to type-check them.
module FsClean.ProjectLoader

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

type Project =
    {
      /// What the compiler is given to type-check the project.
      Options: FSharpProjectOptions
      /// The project file. A project targeting several frameworks is loaded once per framework, so
      /// several `Project`s can share a file.
      File: string
      /// The `<ProjectReference>`s the project declares, as the paths of the referenced project
      /// files. Not what reaches it transitively, and not references that only feed the build
      /// (`ReferenceOutputAssembly="false"`): nothing compiles against those.
      References: string list
    }

let private initLock = obj ()
let mutable private toolsPath = None

/// MSBuild can only be registered once per process, so later calls reuse the first registration.
let private toolsPathFor (directory: string) =
    lock initLock (fun () ->
        match toolsPath with
        | Some existing -> existing
        | None ->
            // Picks the SDK the way `dotnet` would from this directory, so a global.json is respected.
            let created = Init.init (DirectoryInfo directory) None
            toolsPath <- Some created
            created)

/// What MSBuild makes of the project's own `<ProjectReference>` items for one target framework, so
/// conditions and imported props count. Ionide's `ReferencedProjects` isn't this: it also lists
/// the projects that reach a project only through the ones it references.
let private declaredReferences (projectFile: string) (targetFramework: string) : string list =
    let collection = new Microsoft.Build.Evaluation.ProjectCollection()

    try
        let globalProperties = Dictionary<string, string>()

        if not (System.String.IsNullOrEmpty targetFramework) then
            globalProperties["TargetFramework"] <- targetFramework

        let project = Microsoft.Build.Evaluation.Project(projectFile, globalProperties, null, collection)

        project.GetItems "ProjectReference"
        |> Seq.filter (fun item -> item.GetMetadataValue "ReferenceOutputAssembly" <> "false")
        |> Seq.map (fun item -> Path.GetFullPath(item.GetMetadataValue "FullPath"))
        |> Seq.toList
    finally
        collection.UnloadAllProjects()
        collection.Dispose()

/// The projects must already be restored: MSBuild is asked for the design-time compiler arguments,
/// which needs `obj/project.assets.json`.
let load (projectPaths: string list) : Project list =
    let paths = projectPaths |> List.map Path.GetFullPath
    let loader = WorkspaceLoader.Create(toolsPathFor (Path.GetDirectoryName paths.Head))

    // Also returns the projects the requested ones reference, which is what lets uses in a
    // consumer keep declarations alive in the library it calls.
    let projects =
        try
            loader.LoadProjects paths |> Seq.toList
        with :? FileNotFoundException as error when error.Message.Contains "System.Runtime, Version=" ->
            // MSBuild from an SDK for a newer .NET than this process runs on can't be loaded into it.
            let needed = Regex.Match(error.Message, @"System\.Runtime, Version=(\d+)")
            let wanted = if needed.Success then needed.Groups[1].Value else "a newer version of"

            failwithf
                "The SDK these projects pin (global.json) is for .NET %s, but fsclean is running on .NET %d. Install the .NET %s runtime and run fsclean on it, e.g. with DOTNET_ROLL_FORWARD_TO_PRERELEASE=1 set if it's a preview."
                wanted
                Environment.Version.Major
                wanted

    if projects.IsEmpty then
        failwithf "MSBuild returned no projects for: %s" (String.concat ", " paths)

    projects
    |> List.map (fun project ->
        { Options = FCS.mapToFSharpProjectOptions project projects
          File = Path.GetFullPath project.ProjectFileName
          References = declaredReferences project.ProjectFileName project.TargetFramework })
