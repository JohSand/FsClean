/// Reads declaration extents out of the untyped syntax tree.
///
/// The typed symbol API tells us *what* refers to *what*, but only gives a declaration's identifier
/// range. The syntax tree is where the full source extent of a `let`, type or member lives, which
/// is what both ownership lookups ("which declaration is this use inside of?") and removal need.
module FsClean.Ast

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type ExtentKind =
    | LetBinding
    | Member
    | Type
    | Exception

/// One declaration of a chain introduced by a single keyword: `let rec a ... and b ...` or
/// `type A = ... and B = ...`. Deleting the first one isn't like deleting a lone declaration, since
/// the next one's `and` would have to become the keyword.
type Chain =
    {
      /// The whole chain, from its first keyword to the end of its last declaration.
      Whole: range
      Size: int
      /// Where this declaration sits in the chain, from 0.
      Index: int
    }

/// A nested `module X = ...`. It isn't something the analysis judges, but it can't be left behind
/// with nothing in it: once all of its declarations are removed, it goes too.
type Container =
    {
      Name: string
      /// The whole module, from its attributes to its last declaration.
      Range: range
      /// How many declarations its body has, of any kind, `open`s and `do`s included.
      Size: int
      /// The module around it, when it is nested too.
      Outer: Container option
    }

type Extent =
    { Kind: ExtentKind
      /// The whole declaration: its leading keyword (`let`, `and`, `member`, `type`, ...) and any
      /// attributes above it. Doc comments aren't part of the syntax tree, so they're not here.
      Range: range
      /// Set for the declarations of a `let rec ... and ...` or `type ... and ...` chain.
      Chain: Chain option
      /// The nested module a module-level declaration sits in. None for members, and for
      /// declarations directly in a namespace or a file's own module.
      Container: Container option
      /// False when evaluating the declaration can have an observable effect. Only meaningful for
      /// module-level `let` bindings and `member val`, the two places where a definition runs code
      /// just by existing.
      IsPure: bool }

let rec private isPureExpr (expr: SynExpr) =
    match expr with
    | SynExpr.Const _
    | SynExpr.Null _
    | SynExpr.Lambda _
    | SynExpr.Ident _
    | SynExpr.LongIdent _ -> true
    | SynExpr.Paren(expr = inner)
    | SynExpr.Typed(expr = inner) -> isPureExpr inner
    | SynExpr.Tuple(exprs = exprs)
    | SynExpr.ArrayOrList(exprs = exprs) -> List.forall isPureExpr exprs
    | _ -> false

/// `let f x = ...` only defines something; `let x = <expr>` also runs <expr>.
let private isPureBinding (SynBinding(headPat = pat; expr = body)) =
    let definesFunction =
        match pat with
        | SynPat.LongIdent(argPats = SynArgPats.Pats(_ :: _)) -> true
        | _ -> false

    definesFunction || isPureExpr body

/// Attributes sit above the declaration's keyword, outside the range the parser gives it.
let private withAttributes (range: range) (attributes: SynAttributes) =
    attributes
    |> List.fold (fun (whole: range) (list: SynAttributeList) -> Range.unionRanges whole list.Range) range

/// A synthesized keyword has no position in this file.
let private withKeyword (range: range) (keyword: range) =
    if keyword.FileName = range.FileName then
        Range.unionRanges range keyword
    else
        range

let private bindingAttributes (SynBinding(attributes = attributes)) = attributes

let private letExtent range container (binding: SynBinding) =
    { Kind = LetBinding
      Range = withAttributes range (bindingAttributes binding)
      Chain = None
      Container = container
      IsPure = isPureBinding binding }

/// Gives each extent of a chain the whole chain's range and its own position in it.
let private inChain (extents: Extent list) =
    let whole = extents |> List.map (fun e -> e.Range) |> List.reduce Range.unionRanges

    extents
    |> List.mapi (fun i extent ->
        { extent with
            Chain =
              Some
                  { Whole = whole
                    Size = extents.Length
                    Index = i } })

let rec private memberDefn (defn: SynMemberDefn) : Extent list =
    match defn with
    | SynMemberDefn.Member(memberDefn = binding; range = range) ->
        [ { Kind = Member
            Range = withAttributes range (bindingAttributes binding)
            Chain = None
            Container = None
            IsPure = true } ]
    | SynMemberDefn.GetSetMember(memberDefnForGet = getter; memberDefnForSet = setter; range = range) ->
        [ { Kind = Member
            Range = [ getter; setter ] |> List.choose id |> List.collect bindingAttributes |> withAttributes range
            Chain = None
            Container = None
            IsPure = true } ]
    | SynMemberDefn.AutoProperty(attributes = attributes; range = range) ->
        // `member val X = expr` runs `expr` in the constructor.
        [ { Kind = Member
            Range = withAttributes range attributes
            Chain = None
            Container = None
            IsPure = false } ]
    | SynMemberDefn.Interface(members = Some members) -> List.collect memberDefn members
    | _ -> []

let private typeAttributes (SynTypeDefn(typeInfo = SynComponentInfo(attributes = attributes))) = attributes

let private typeExtent (range: range) container (defn: SynTypeDefn) =
    { Kind = Type
      Range = withAttributes range (typeAttributes defn)
      Chain = None
      Container = container
      IsPure = true }

let private typeMembers (SynTypeDefn(typeRepr = repr; members = members)) =
    let reprMembers =
        match repr with
        | SynTypeDefnRepr.ObjectModel(members = members) -> members
        | _ -> []

    List.collect memberDefn (reprMembers @ members)

let rec private moduleDecl (container: Container option) (decl: SynModuleDecl) : Extent list =
    match decl with
    | SynModuleDecl.Let(bindings = [ binding ]; range = range) -> [ letExtent range container binding ]
    | SynModuleDecl.Let(bindings = bindings) ->
        // `let rec a ... and b ...`: each binding stands on its own, from its `let rec` or `and`.
        bindings
        |> List.map (fun (SynBinding(trivia = trivia) as binding) ->
            let range = withKeyword binding.RangeOfBindingWithRhs trivia.LeadingKeyword.Range
            letExtent range container binding)
        |> inChain
    | SynModuleDecl.Types(typeDefns = [ defn ]; range = range) ->
        typeExtent range container defn :: typeMembers defn
    | SynModuleDecl.Types(typeDefns = defns) ->
        let types =
            defns
            |> List.map (fun (SynTypeDefn(range = range; trivia = trivia) as defn) ->
                typeExtent (withKeyword range trivia.LeadingKeyword.Range) container defn)
            |> inChain

        List.zip types defns
        |> List.collect (fun (extent, defn) -> extent :: typeMembers defn)
    | SynModuleDecl.Exception(exnDefn = SynExceptionDefn(exnRepr = SynExceptionDefnRepr(attributes = attributes)); range = range) ->
        [ { Kind = Exception
            Range = withAttributes range attributes
            Chain = None
            Container = container
            IsPure = true } ]
    | SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(attributes = attributes; longId = name); decls = decls; range = range) ->
        let inner =
            Some
                { Name = (name |> List.last).idText
                  Range = withAttributes range attributes
                  Size = decls.Length
                  Outer = container }

        List.collect (moduleDecl inner) decls
    | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(decls = decls)) -> List.collect (moduleDecl container) decls
    | _ -> []

/// Every declaration extent in a file. Signature files contribute nothing.
let extents (input: ParsedInput) : Extent list =
    match input with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        modules |> List.collect (fun (SynModuleOrNamespace(decls = decls)) -> List.collect (moduleDecl None) decls)
    | ParsedInput.SigFile _ -> []
