/// Finds project references that nothing compiles against.
///
/// A `<ProjectReference>` is needed at compile time when some symbol use in the referencing project
/// resolves to a declaration in a source file of the referenced one. That is the evidence the
/// dead-code pass already works from, with the project boundary kept instead of thrown away.
///
/// It is only compile-time evidence. A reference nothing compiles against can still matter at
/// runtime (a plugin loaded by reflection, a project referenced so that its output gets copied), so
/// `Unneeded` means "no compile-time use", not "safe to remove" on its own.
module FsClean.References

open System.Collections.Generic
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FsClean.ProjectLoader

type Verdict =
    /// Something declared in the referenced project is used.
    | Needed
    /// Nothing declared in the referenced project, or in anything it references, is used.
    | Unneeded
    /// Nothing declared in it is used, but these projects are, and the referencing project reaches
    /// them only through this reference. Reference them directly, or keep this as the route.
    | OnlyTransitive of provides: string list
    /// Not an F# project that was loaded, so there are no sources to attribute uses to.
    | NotAnalyzed

type Finding =
    {
      /// The referencing project's file.
      Project: string
      /// The referenced project's file.
      Reference: string
      /// Judged by every use.
      Now: Verdict
      /// Judged by the uses in live code only: what stays true once the dead code is gone.
      AfterCleanup: Verdict
      /// A few of the uses behind the verdict, as `Symbol (file:line)`.
      Evidence: string list
    }

/// What a project calls its references in reports.
let name (projectFile: string) = Path.GetFileNameWithoutExtension projectFile

/// What one project uses of another.
type private Crossing() =
    member val Uses = 0 with get, set
    member val LiveUses = 0 with get, set
    member val LiveExamples = ResizeArray<string * string>()
    member val DeadExamples = ResizeArray<string * string>()

let private maxExamples = 3

let private symbolName (symbol: FSharpSymbol) =
    try
        symbol.FullName
    with _ ->
        symbol.DisplayName

/// A namespace is declared wherever a file opens it, in any project, so the compiler resolves it to
/// an arbitrary one of them (typically a generated AssemblyInfo.fs). Using one says nothing about
/// which project it came from.
let private isNamespace (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as entity -> entity.IsNamespace
    | _ -> false

/// Modules count as uses, but make poor evidence: `Core` says less than the `Core.used` beside it.
let private isModule (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as entity -> entity.IsFSharpModule
    | _ -> false

let private addExample (examples: ResizeArray<string * string>) (symbolUse: FSharpSymbolUse) =
    if examples.Count < maxExamples && not (isModule symbolUse.Symbol) then
        let symbol = symbolName symbolUse.Symbol

        if not (examples.Exists(fun (existing, _) -> existing = symbol)) then
            examples.Add(symbol, $"{symbol} ({Path.GetFileName symbolUse.Range.FileName}:{symbolUse.Range.StartLine})")

/// `usesByProject` pairs each project file with the symbol uses the compiler reported for it,
/// once per target framework if it has several. `isLive` says whether a use sits in code that
/// survives dead-code removal.
let find
    (projects: Project list)
    (usesByProject: (string * FSharpSymbolUse[]) list)
    (isLive: range -> bool)
    : Finding list =
    // Which project each source file belongs to. A file linked into several projects belongs to all.
    let owners = Dictionary<string, HashSet<string>>()

    for project in projects do
        for file in project.Options.SourceFiles do
            match owners.TryGetValue file with
            | true, files -> files.Add project.File |> ignore
            | _ -> owners[file] <- HashSet [ project.File ]

    // from project -> to project -> what it uses
    let crossings = Dictionary<string, Dictionary<string, Crossing>>()

    let crossing (from: string) (target: string) =
        let targets =
            match crossings.TryGetValue from with
            | true, existing -> existing
            | _ ->
                let created = Dictionary<string, Crossing>()
                crossings[from] <- created
                created

        match targets.TryGetValue target with
        | true, existing -> existing
        | _ ->
            let created = Crossing()
            targets[target] <- created
            created

    let ownedBy (project: string) (file: string) =
        match owners.TryGetValue file with
        | true, files -> files.Contains project
        | _ -> false

    for from, uses in usesByProject do
        for symbolUse in uses do
            // Uses in this project's own files only; a definition isn't a use.
            if
                not symbolUse.IsFromDefinition
                && not (isNamespace symbolUse.Symbol)
                && ownedBy from symbolUse.Range.FileName
            then
                match symbolUse.Symbol.DeclarationLocation with
                | Some declared ->
                    match owners.TryGetValue declared.FileName with
                    | true, declaring when not (declaring.Contains from) ->
                        let live = isLive symbolUse.Range

                        for target in declaring do
                            let crossing = crossing from target
                            crossing.Uses <- crossing.Uses + 1

                            if live then
                                crossing.LiveUses <- crossing.LiveUses + 1

                            addExample (if live then crossing.LiveExamples else crossing.DeadExamples) symbolUse
                    | _ -> ()
                | None -> () // Declared outside the loaded projects: a package, the framework.

    let projectFiles = projects |> List.map (fun project -> project.File) |> List.distinct
    let analyzed = Set.ofList projectFiles

    let direct =
        projects
        |> List.groupBy (fun project -> project.File)
        |> List.map (fun (file, group) -> file, group |> List.collect (fun project -> project.References) |> List.distinct)
        |> Map.ofList

    // Everything reachable through references, however indirectly.
    let reachable = Dictionary<string, Set<string>>()

    let rec closure (file: string) : Set<string> =
        match reachable.TryGetValue file with
        | true, cached -> cached
        | _ ->
            let result =
                direct
                |> Map.tryFind file
                |> Option.defaultValue []
                |> List.fold (fun all reference -> all |> Set.add reference |> Set.union (closure reference)) Set.empty

            reachable[file] <- result
            result

    let usedBy (project: string) (counts: Crossing -> bool) =
        match crossings.TryGetValue project with
        | true, targets ->
            targets
            |> Seq.filter (fun (KeyValue(_, crossing)) -> counts crossing)
            |> Seq.map (fun (KeyValue(target, _)) -> target)
            |> Set.ofSeq
        | _ -> Set.empty

    let verdictOf (project: string) (used: Set<string>) (reference: string) =
        if not (analyzed.Contains reference) then
            NotAnalyzed
        elif used.Contains reference then
            Needed
        else
            let directRefs = Set.ofList direct[project]

            // What the references that are used anyway already carry along.
            let alreadyReachable =
                directRefs |> Set.filter used.Contains |> Seq.collect closure |> Set.ofSeq

            let provides =
                used
                |> Set.filter (fun target ->
                    not (directRefs.Contains target)
                    && not (alreadyReachable.Contains target)
                    && (closure reference).Contains target)

            if provides.IsEmpty then
                Unneeded
            else
                OnlyTransitive(Set.toList provides)

    let examplesOf (project: string) (reference: string) =
        match crossings.TryGetValue project with
        | true, targets ->
            match targets.TryGetValue reference with
            | true, crossing ->
                (if crossing.LiveExamples.Count > 0 then crossing.LiveExamples else crossing.DeadExamples)
                |> Seq.map snd
                |> Seq.toList
            | _ -> []
        | _ -> []

    [ for project in projectFiles do
          let usedNow = usedBy project (fun crossing -> crossing.Uses > 0)
          let usedAfterCleanup = usedBy project (fun crossing -> crossing.LiveUses > 0)

          for reference in direct[project] do
              let now = verdictOf project usedNow reference

              yield
                  { Project = project
                    Reference = reference
                    Now = now
                    AfterCleanup = verdictOf project usedAfterCleanup reference
                    Evidence =
                      match now with
                      | Needed -> examplesOf project reference
                      | OnlyTransitive provides -> provides |> List.collect (examplesOf project >> List.truncate 1)
                      | Unneeded
                      | NotAnalyzed -> [] } ]
