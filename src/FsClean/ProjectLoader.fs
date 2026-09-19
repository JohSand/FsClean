/// Turns `.fsproj` files into the options FSharp.Compiler.Service needs to type-check them.
module FsClean.ProjectLoader

open System.Collections.Generic
open System.IO
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
    let projects = loader.LoadProjects paths |> Seq.toList

    if projects.IsEmpty then
        failwithf "MSBuild returned no projects for: %s" (String.concat ", " paths)

    projects
    |> List.map (fun project ->
        { Options = FCS.mapToFSharpProjectOptions project projects
          File = Path.GetFullPath project.ProjectFileName
          References = declaredReferences project.ProjectFileName project.TargetFramework })
