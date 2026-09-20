/// Finds the declarations nothing live can reach.
///
/// Every module-level `let`, type, exception and member is a node. Each symbol use the compiler
/// reports becomes an edge from the node the use sits inside to the node the used symbol is
/// declared in. Whatever can't be reached from a root (entry point, public API, ...) is dead.
/// Reachability, rather than "has zero references", is what lets this see dead code that is only
/// kept alive by other dead code, including cycles.
module FsClean.Analysis

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FsClean.Ast
open FsClean.ProjectLoader

type Options =
    {
      /// Treat only entry points as roots, even in libraries. Right when every consumer of the
      /// libraries is part of the analysis; otherwise public API would be reported as dead.
      WholeProgram: bool
      /// Also explain every declaration whose name contains this text: what uses it, what it
      /// uses, and, if it's alive, the chain from a root that keeps it there.
      Explain: string option
    }

type Decl =
    { Name: string
      Kind: string
      /// The whole declaration, including its leading keyword and attributes.
      Range: range
      Extent: Extent
      /// Dead declarations that use each other share a number: a helper can't go while its dead
      /// caller stays, and a dead cycle can only go all at once. -1 for anything not reported dead.
      Component: int
      /// Why removing it would leave broken code that the compiler can't be relied on to explain,
      /// when it would.
      Blocker: string option }

type Report =
    {
      /// How many declarations were considered.
      Analyzed: int
      /// Unreachable from every root. Members of a dead type aren't listed separately.
      Dead: Decl list
      /// Unused, but the initializer might have effects. Kept alive, and listed for a human to decide.
      NeedsReview: Decl list
      Explanations: string list
      /// What each analyzed project's `<ProjectReference>`s are used for, needed ones included.
      References: References.Finding list
    }

// ---- graph -----------------------------------------------------------------

type private Node(extent: Extent) =
    member _.Extent = extent
    /// What this declaration introduces. Attributes, accessibility and the like are read from
    /// these, so they must never include a symbol that is only named here.
    member val Symbols = ResizeArray<FSharpSymbol>()
    /// The types a `type X with ...` block names without declaring.
    member val Extends = ResizeArray<FSharpEntity>()
    member val IsLibrary = false with get, set
    member val Parent: Node option = None with get, set

let private sameNode (a: Node) (b: Node) = obj.ReferenceEquals(a, b)

/// Nodes grouped by file, so a source position can be resolved to the declaration it's inside of.
type private NodeIndex(nodes: Node seq) =
    let byFile =
        nodes
        |> Seq.groupBy (fun node -> node.Extent.Range.FileName)
        |> dict

    /// The narrowest node containing the range, ignoring `except`.
    member _.Innermost(range: range, ?except: Node) : Node option =
        match byFile.TryGetValue range.FileName with
        | true, candidates ->
            candidates
            |> Seq.filter (fun node ->
                Range.rangeContainsRange node.Extent.Range range
                && (match except with
                    | Some e -> not (sameNode node e)
                    | None -> true))
            |> Seq.fold
                (fun best node ->
                    match best with
                    | Some(b: Node) when Range.rangeContainsRange b.Extent.Range node.Extent.Range -> Some node
                    | Some _ -> best
                    | None -> Some node)
                None
        | _ -> None

// ---- what the compiler can't see -------------------------------------------

/// Attributes that describe how something compiles rather than who consumes it. Any other
/// attribute is assumed to mean a framework or reflection reaches the declaration (a test
/// runner, a serializer, ...), which makes it a root.
let private neutralAttributes =
    HashSet
        [ "Struct"
          "Class"
          "Interface"
          "AbstractClass"
          "Sealed"
          "RequireQualifiedAccess"
          "AutoOpen"
          "CLIMutable"
          "NoComparison"
          "NoEquality"
          "CustomComparison"
          "CustomEquality"
          "ReferenceEquality"
          "StructuralEquality"
          "StructuralComparison"
          "Literal"
          "DefaultValue"
          "Obsolete"
          "CompiledName"
          "CompilationMapping"
          "CompilationRepresentation"
          "AllowNullLiteral"
          "Measure"
          "TailCall"
          "GeneralizableValue" ]

/// Members the compiler calls on the programmer's behalf (computation expression desugaring,
/// `for ... in`, implicit conversions, ...) without reporting a symbol use for them.
let private implicitlyInvokedMembers =
    HashSet
        [ "Bind"
          "BindReturn"
          "Return"
          "ReturnFrom"
          "ReturnFromFinal"
          "Yield"
          "YieldFrom"
          "YieldFromFinal"
          "Zero"
          "Combine"
          "Delay"
          "Run"
          "For"
          "While"
          "Using"
          "TryWith"
          "TryFinally"
          "MergeSources"
          "Source"
          "Quote"
          "GetEnumerator"
          "MoveNext"
          "get_Current"
          "op_Implicit"
          "op_Explicit"
          // Frameworks that find a method by name with reflection: ASP.NET's middleware
          // convention (`UseMiddleware<T>()` calls `Invoke` or `InvokeAsync`) and a Startup class.
          "Invoke"
          "InvokeAsync"
          "Configure"
          "ConfigureServices"
          "ConfigureContainer" ]

/// The numbered forms that `and!` desugars to: `Bind2`, `Bind3Return`, `MergeSources4`, ...
let private andBangMember = Regex(@"^(Bind|MergeSources)\d+$|^Bind\d+Return$", RegexOptions.Compiled)

let private isImplicitlyInvokedName name =
    implicitlyInvokedMembers.Contains name || andBangMember.IsMatch name

let private attributeNames (symbol: FSharpSymbol) =
    let attributes =
        match symbol with
        | :? FSharpEntity as entity -> entity.Attributes
        | :? FSharpMemberOrFunctionOrValue as m -> m.Attributes
        | _ -> ResizeArray()

    attributes
    |> Seq.map (fun attribute ->
        let name = attribute.AttributeType.LogicalName
        if name.EndsWith "Attribute" then name[.. name.Length - 10] else name)
    |> Seq.toList

let private isEntryPoint symbol =
    attributeNames symbol |> List.contains "EntryPoint"

let private hasRootingAttribute symbol =
    attributeNames symbol
    |> List.exists (fun name -> name <> "EntryPoint" && not (neutralAttributes.Contains name))

let rec private isPublicEntity (entity: FSharpEntity) =
    entity.Accessibility.IsPublic
    && (match entity.DeclaringEntity with
        | Some parent -> isPublicEntity parent
        | None -> true)

/// Public, and reachable from outside through a chain of public containers.
let private isEffectivelyPublic (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as entity -> isPublicEntity entity
    | :? FSharpMemberOrFunctionOrValue as m ->
        m.Accessibility.IsPublic
        && (match m.DeclaringEntity with
            | Some parent -> isPublicEntity parent
            | None -> true)
    | _ -> false

/// The members an inline function demands of its type arguments: `List.sum` wants `get_Zero` and
/// `op_Addition`, and the compiler solves those against the argument type without reporting a use.
let private requiredMembers (m: FSharpMemberOrFunctionOrValue) =
    try
        [ for parameter in m.GenericParameters do
              for requirement in parameter.Constraints do
                  if requirement.IsMemberConstraint then
                      yield requirement.MemberConstraintData.MemberName ]
    with _ ->
        []

/// FsCheck registers the static members of a type it's given that return `Arbitrary<_>`, by reflection.
let private returnsArbitrary (m: FSharpMemberOrFunctionOrValue) =
    try
        m.ReturnParameter.Type.HasTypeDefinition
        && m.ReturnParameter.Type.TypeDefinition.DisplayName = "Arbitrary"
        && m.ReturnParameter.Type.TypeDefinition.AccessPath = "FsCheck"
    with _ ->
        false

/// Reached through dispatch, desugaring, or a member constraint, so it's live whenever its type is.
/// `required` is what the inline functions in use ask of their type arguments.
let private isImplicitlyInvoked (required: HashSet<string>) (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpMemberOrFunctionOrValue as m ->
        m.IsOverrideOrExplicitInterfaceImplementation
        || m.IsDispatchSlot
        || isImplicitlyInvokedName m.LogicalName
        || (m.IsMember && returnsArbitrary m)
        || (m.IsMember
            && (required.Contains m.LogicalName
                || required.Contains("get_" + m.LogicalName)
                || required.Contains("set_" + m.LogicalName)))
    | _ -> false

// ---- symbols ---------------------------------------------------------------

/// The symbols a node can be named by: types and exceptions, and anything that lives in a module
/// or type. Locals, parameters and the like are inside a node, never a node.
let private isDeclaration (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as entity -> not entity.IsNamespace && not entity.IsFSharpModule
    | :? FSharpMemberOrFunctionOrValue as m -> m.IsModuleValueOrMember
    | _ -> false

let private safe fallback f =
    try
        f ()
    with _ ->
        fallback

/// Types and exceptions are what a declaration is called; a constructor shares its type's name.
/// A `type X with` block declares nothing, so it's called after the type it extends.
let private primarySymbol (node: Node) : FSharpSymbol =
    let symbols = node.Symbols |> Seq.toList

    let nonConstructor =
        symbols
        |> List.tryFind (fun symbol ->
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as m -> not m.IsConstructor
            | _ -> true)

    symbols
    |> List.tryFind (fun symbol -> symbol :? FSharpEntity)
    |> Option.orElse nonConstructor
    |> Option.orElse (List.tryHead symbols)
    |> Option.orElseWith (fun () -> node.Extends |> Seq.tryHead |> Option.map (fun entity -> entity :> FSharpSymbol))
    |> Option.get

let private kindOf (symbol: FSharpSymbol) =
    match symbol with
    | :? FSharpEntity as e ->
        if e.IsFSharpRecord then "record"
        elif e.IsFSharpUnion then "union"
        elif e.IsFSharpExceptionDeclaration then "exception"
        elif e.IsInterface then "interface"
        elif e.IsEnum then "enum"
        elif e.IsClass then "class"
        else "type"
    | :? FSharpMemberOrFunctionOrValue as m ->
        if m.IsConstructor then "constructor"
        elif m.IsProperty || m.IsPropertyGetterMethod || m.IsPropertySetterMethod then "property"
        elif m.IsMember then "member"
        elif m.CurriedParameterGroups.Count > 0 then "function"
        else "value"
    | _ -> "declaration"

let private describe (node: Node) : Decl =
    let symbol = primarySymbol node

    { Name = safe symbol.DisplayName (fun () -> symbol.FullName)
      Kind = if node.Symbols.Count = 0 then "extension" else kindOf symbol
      Range = node.Extent.Range
      Extent = node.Extent
      Component = -1
      Blocker = None }

// ---- analysis --------------------------------------------------------------

let private isExe (project: FSharpProjectOptions) =
    project.OtherOptions
    |> Array.exists (fun option -> option = "--target:exe" || option = "--target:winexe")

let sourceFiles (project: FSharpProjectOptions) =
    project.SourceFiles
    |> Array.filter (fun file -> file.EndsWith(".fs", StringComparison.OrdinalIgnoreCase))

let errorsIn (results: FSharpCheckProjectResults) =
    results.Diagnostics
    |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    |> Array.map (fun d -> $"{d.FileName}({d.StartLine},{d.StartColumn}): {d.Message}")
    |> Array.toList

/// The compiler keeps three projects in its cache unless told otherwise. A solution's projects
/// reference each other and are checked one after another, so with fewer slots than projects it
/// would check the same ones over and over.
/// FSCLEAN_PROJECT_CACHE sets the number of slots, to trade time for memory on a big solution.
let createChecker (projects: Project list) =
    let slots =
        match Int32.TryParse(Environment.GetEnvironmentVariable "FSCLEAN_PROJECT_CACHE") with
        | true, slots when slots > 0 -> slots
        | _ -> max 3 projects.Length

    FSharpChecker.Create(projectCacheSize = slots)

/// What the compiler reports for the projects as they are on disk right now.
let typeErrors (checker: FSharpChecker) (projects: Project list) : Async<string list> =
    async {
        let errors = ResizeArray<string>()

        for project in projects do
            let! results = checker.ParseAndCheckProject project.Options
            errors.AddRange(errorsIn results)

        return Seq.toList errors
    }

/// One node per declaration, deduplicated by extent so a file shared between projects counts once.
let private parseNodes (checker: FSharpChecker) (projects: Project list) =
    async {
        let nodes = Dictionary<range, Node>(HashIdentity.Structural)

        for project in projects do
            let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions project.Options

            for file in sourceFiles project.Options do
                let! parsed = checker.ParseFile(file, SourceText.ofString (File.ReadAllText file), parsingOptions)

                for extent in extents parsed.ParseTree do
                    let node =
                        match nodes.TryGetValue extent.Range with
                        | true, existing -> existing
                        | _ ->
                            let created = Node extent
                            nodes[extent.Range] <- created
                            created

                    if not (isExe project.Options) then
                        node.IsLibrary <- true

        return nodes.Values |> Seq.toList
    }

/// Like `analyze`, telling `log` how long each phase took.
let analyzeWith
    (log: string -> unit)
    (checker: FSharpChecker)
    (options: Options)
    (projects: Project list)
    : Async<Result<Report, string list>> =
    async {
        let clock = Diagnostics.Stopwatch.StartNew()
        let mutable last = TimeSpan.Zero

        let mark (what: string) =
            log $"  {what}: {(clock.Elapsed - last).TotalSeconds:F1}s"
            last <- clock.Elapsed

        let! allNodes = parseNodes checker projects
        mark $"parsed {allNodes.Length} declarations"

        let uses = ResizeArray<FSharpSymbolUse>()
        let usesByProject = ResizeArray<string * FSharpSymbolUse[]>()
        let errors = ResizeArray<string>()

        for project in projects do
            let! results = checker.ParseAndCheckProject project.Options
            errors.AddRange(errorsIn results)

            let projectUses = results.GetAllUsesOfAllSymbols()
            usesByProject.Add((project.File, projectUses))
            uses.AddRange projectUses
            mark $"type-checked {References.name project.File} ({projectUses.Length} uses)"

        // A remover working from a half-typed program would miss uses and delete live code.
        if errors.Count > 0 then
            return Error(Seq.toList errors)
        else
            // Name each node after the declarations found inside it, then drop the extents that
            // declare nothing (`do`, `let _ = ...`, type augmentations): uses inside those are
            // top-level code, not something a declaration owns.
            let everyExtent = NodeIndex allNodes

            for symbolUse in uses do
                if symbolUse.IsFromDefinition && isDeclaration symbolUse.Symbol then
                    match everyExtent.Innermost symbolUse.Range with
                    | None -> ()
                    | Some node ->
                        let declaredHere =
                            symbolUse.Symbol.DeclarationLocation
                            |> Option.exists (fun location -> Range.rangeContainsRange node.Extent.Range location)

                        match symbolUse.Symbol with
                        | symbol when declaredHere -> node.Symbols.Add symbol
                        // `type X with` marks X as a definition, though X is declared elsewhere.
                        | :? FSharpEntity as entity -> node.Extends.Add entity
                        | _ -> ()

            let nodes =
                allNodes
                |> List.filter (fun node -> node.Symbols.Count > 0 || node.Extends.Count > 0)
            let index = NodeIndex nodes
            mark $"attached symbols to {nodes.Length} declarations"

            for node in nodes do
                node.Parent <- index.Innermost(node.Extent.Range, except = node)

            let edges = Dictionary<Node, HashSet<Node>>(HashIdentity.Reference)
            let roots = Dictionary<Node, string>(HashIdentity.Reference)

            let addEdge (source: Node) (target: Node) =
                if not (sameNode source target) then
                    let targets =
                        match edges.TryGetValue source with
                        | true, existing -> existing
                        | _ ->
                            let created = HashSet<Node>(HashIdentity.Reference)
                            edges[source] <- created
                            created

                    targets.Add target |> ignore

            let addRoot (node: Node) reason = roots.TryAdd(node, reason) |> ignore

            // Symbol uses: the node a use sits in depends on the node its symbol is declared in.
            for symbolUse in uses do
                if not symbolUse.IsFromDefinition then
                    let target =
                        symbolUse.Symbol.DeclarationLocation
                        |> Option.bind (fun declaration -> index.Innermost declaration)

                    match target with
                    | None -> () // Declared outside the analyzed code, e.g. in a NuGet package.
                    | Some target ->
                        match index.Innermost symbolUse.Range with
                        | Some owner -> addEdge owner target
                        | None -> addRoot target "referenced from top-level code"

            mark "linked uses to declarations"

            // A `type X with ...` block declares no type of its own, so nothing ever names it. It
            // stays alive with the type it extends; when that type lives elsewhere (a NuGet package,
            // FSharp.Core) nobody in view can say who uses the block, so it's a root.
            for node in nodes do
                for entity in node.Extends do
                    let original =
                        (entity :> FSharpSymbol).DeclarationLocation
                        |> Option.bind (fun location -> index.Innermost location)

                    match original with
                    | Some original -> addEdge original node
                    | None -> addRoot node "extends a type declared outside the analyzed code"

            // Every member name some inline function in use asks its type arguments for.
            let required = HashSet<string>()
            let inspected = HashSet<FSharpSymbol>()

            for symbolUse in uses do
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as m when not symbolUse.IsFromDefinition && inspected.Add m ->
                    required.UnionWith(requiredMembers m)
                | _ -> ()

            // Members reached without a named use, and members keeping their type alive.
            for node in nodes do
                match node.Parent with
                | Some parent when node.Extent.Kind = Member ->
                    addEdge node parent

                    if not node.Extent.IsPure || node.Symbols |> Seq.exists (isImplicitlyInvoked required) then
                        addEdge parent node
                | _ -> ()

            for node in nodes do
                if node.Symbols |> Seq.exists isEntryPoint then
                    addRoot node "entry point"

                if node.Symbols |> Seq.exists hasRootingAttribute then
                    addRoot node "attribute"

                if
                    not options.WholeProgram
                    && node.IsLibrary
                    && node.Symbols |> Seq.exists isEffectivelyPublic
                then
                    addRoot node "public API"

            // Last, so a more specific reason wins: this one only means "don't delete blindly".
            let impureReason = "initializer may have side effects"

            for node in nodes do
                if node.Extent.Kind = LetBinding && not node.Extent.IsPure then
                    addRoot node impureReason

            // What can be reached from the roots without going through `excluded`.
            let search (excluded: HashSet<Node>) =
                let reachable = HashSet<Node>(HashIdentity.Reference)
                let reachedFrom = Dictionary<Node, Node option>(HashIdentity.Reference)
                let pending = Queue<Node * Node option>(roots.Keys |> Seq.map (fun root -> root, None))

                while pending.Count > 0 do
                    let node, from = pending.Dequeue()

                    if not (excluded.Contains node) && reachable.Add node then
                        reachedFrom[node] <- from

                        match edges.TryGetValue node with
                        | true, targets -> targets |> Seq.iter (fun target -> pending.Enqueue(target, Some node))
                        | _ -> ()

                reachable, reachedFrom

            let firstPass, _ = search (HashSet<Node>(HashIdentity.Reference))

            // A `type X with` block is alive only because it can't be seen to be unused, but if every
            // member in it is dead nothing is left to keep: the block goes, with them.
            let hollow = HashSet<Node>(HashIdentity.Reference)

            for node in nodes do
                if node.Symbols.Count = 0 && firstPass.Contains node then
                    let members = nodes |> List.filter (fun other -> other.Parent |> Option.exists (sameNode node))

                    if not members.IsEmpty && members |> List.forall (fun other -> not (firstPass.Contains other)) then
                        hollow.Add node |> ignore

            let reachable, reachedFrom = search hollow

            // A use in dead code goes when that code does, and takes with it the need for a
            // reference that only it justified.
            let isLive (range: range) =
                match index.Innermost range with
                | Some owner -> reachable.Contains owner
                | None -> true // top-level code, which always runs

            mark "reachability"
            let references = References.find projects (Seq.toList usesByProject) isLive
            mark "project references"

            let usedByLiveCode = HashSet<Node>(HashIdentity.Reference)

            for KeyValue(owner, targets) in edges do
                if reachable.Contains owner then
                    usedByLiveCode.UnionWith targets

            let positionOf (node: Node) =
                node.Extent.Range.FileName, node.Extent.Range.StartLine, node.Extent.Range.StartColumn

            // A dead member goes with its dead type, so it's the type that its links count for.
            let rec top (node: Node) =
                match node.Parent with
                | Some parent when not (reachable.Contains parent) -> top parent
                | _ -> node

            let links = Dictionary<Node, Node>(HashIdentity.Reference)

            let rec representative (node: Node) =
                match links.TryGetValue node with
                | true, next when not (sameNode next node) ->
                    let root = representative next
                    links[node] <- root
                    root
                | _ -> node

            for KeyValue(owner, targets) in edges do
                if not (reachable.Contains owner) then
                    for target in targets do
                        if not (reachable.Contains target) then
                            links[representative (top owner)] <- representative (top target)

            let componentIds = Dictionary<Node, int>(HashIdentity.Reference)

            let componentOf (node: Node) =
                let root = representative (top node)

                match componentIds.TryGetValue root with
                | true, id -> id
                | _ ->
                    let id = componentIds.Count
                    componentIds[root] <- id
                    id

            // Removing the last member of a live type leaves `type T() =` with nothing after the
            // `=`, or a dangling `with`. Which layouts do is easier to avoid than to enumerate.
            let blockerOf (node: Node) =
                match node.Parent with
                | Some parent when reachable.Contains parent ->
                    let siblings = nodes |> List.filter (fun other -> other.Parent |> Option.exists (sameNode parent))

                    if siblings |> List.forall (fun sibling -> not (reachable.Contains sibling)) then
                        Some $"it's the last member of {(describe parent).Name}, and a type can't be left with none"
                    else
                        None
                | _ -> None

            let dead =
                nodes
                |> List.filter (fun node ->
                    not (reachable.Contains node)
                    && (match node.Parent with
                        | Some parent -> reachable.Contains parent
                        | None -> true))
                |> List.sortBy positionOf
                |> List.map (fun node ->
                    { describe node with
                        Component = componentOf node
                        Blocker = blockerOf node })

            let needsReview =
                nodes
                |> List.filter (fun node ->
                    let onlyRootedByInitializer =
                        match roots.TryGetValue node with
                        | true, reason -> reason = impureReason
                        | _ -> false

                    onlyRootedByInitializer && not (usedByLiveCode.Contains node))
                |> List.sortBy positionOf
                |> List.map describe

            let label (node: Node) =
                let decl = describe node
                $"{decl.Name} ({Path.GetFileName decl.Range.FileName}:{decl.Range.StartLine})"

            let explain (node: Node) =
                let range = node.Extent.Range

                let status =
                    if reachable.Contains node then
                        let rec chain (n: Node) =
                            match reachedFrom[n] with
                            | Some previous -> label n :: chain previous
                            | None -> [ $"{label n}  [root: {roots[n]}]" ]

                        "alive, reached via:\n      " + String.concat "\n      <- " (chain node)
                    else
                        "dead"

                let incoming =
                    edges
                    |> Seq.filter (fun (KeyValue(_, targets)) -> targets.Contains node)
                    |> Seq.map (fun (KeyValue(owner, _)) ->
                        let note = if reachable.Contains owner then "" else "  [dead]"
                        $"{label owner}{note}")
                    |> Seq.toList

                let outgoing =
                    match edges.TryGetValue node with
                    | true, targets -> targets |> Seq.map label |> Seq.toList
                    | _ -> []

                let list heading items =
                    match items with
                    | [] -> $"  {heading}: (none)"
                    | _ -> $"  {heading}:\n" + (items |> List.map (fun i -> $"    {i}") |> String.concat "\n")

                [ $"{label node}  lines {range.StartLine}-{range.EndLine}, {node.Extent.Kind}, {node.Symbols.Count} symbol(s)"
                  $"  status: {status}"
                  list "used by" incoming
                  list "uses" outgoing ]
                |> String.concat "\n"

            let explanations =
                match options.Explain with
                | Some needle ->
                    nodes
                    |> List.filter (fun node -> (describe node).Name.Contains(needle, StringComparison.Ordinal))
                    |> List.sortBy positionOf
                    |> List.map explain
                | None -> []

            mark "the rest of the report"

            return
                Ok
                    { Analyzed = nodes.Length
                      Dead = dead
                      NeedsReview = needsReview
                      Explanations = explanations
                      References = references }
    }

let analyze
    (checker: FSharpChecker)
    (options: Options)
    (projects: Project list)
    : Async<Result<Report, string list>> =
    analyzeWith ignore checker options projects
