/// Reads declaration extents out of the untyped syntax tree.
///
/// The typed symbol API tells us *what* refers to *what*, but only gives a declaration's identifier
/// range. The syntax tree is where the full source extent of a `let`, type or member lives, which
/// is what both ownership lookups ("which declaration is this use inside of?") and, later, removal need.
module FsClean.Ast

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type ExtentKind =
    | LetBinding
    | Member
    | Type
    | Exception

type Extent =
    { Kind: ExtentKind
      /// The whole declaration, including its leading keyword.
      Range: range
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

let private letExtent range (binding: SynBinding) =
    { Kind = LetBinding
      Range = range
      IsPure = isPureBinding binding }

let rec private memberDefn (defn: SynMemberDefn) : Extent list =
    match defn with
    | SynMemberDefn.Member(range = range)
    | SynMemberDefn.GetSetMember(range = range) ->
        [ { Kind = Member
            Range = range
            IsPure = true } ]
    | SynMemberDefn.AutoProperty(range = range) ->
        // `member val X = expr` runs `expr` in the constructor.
        [ { Kind = Member
            Range = range
            IsPure = false } ]
    | SynMemberDefn.Interface(members = Some members) -> List.collect memberDefn members
    | _ -> []

let private typeDefn (range: range) (SynTypeDefn(typeRepr = repr; members = members)) =
    let reprMembers =
        match repr with
        | SynTypeDefnRepr.ObjectModel(members = members) -> members
        | _ -> []

    { Kind = Type
      Range = range
      IsPure = true }
    :: List.collect memberDefn (reprMembers @ members)

let rec private moduleDecl (decl: SynModuleDecl) : Extent list =
    match decl with
    | SynModuleDecl.Let(bindings = [ binding ]; range = range) -> [ letExtent range binding ]
    | SynModuleDecl.Let(bindings = bindings) ->
        // `let rec a ... and b ...`: each binding stands on its own.
        bindings |> List.map (fun binding -> letExtent binding.RangeOfBindingWithRhs binding)
    | SynModuleDecl.Types(typeDefns = defns; range = range) ->
        defns
        |> List.mapi (fun i (SynTypeDefn(range = defnRange) as defn) ->
            // Only the first definition of an `and` chain sits right after the `type` keyword.
            typeDefn (if i = 0 then range else defnRange) defn)
        |> List.concat
    | SynModuleDecl.Exception(range = range) ->
        [ { Kind = Exception
            Range = range
            IsPure = true } ]
    | SynModuleDecl.NestedModule(decls = decls) -> List.collect moduleDecl decls
    | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(decls = decls)) -> List.collect moduleDecl decls
    | _ -> []

/// Every declaration extent in a file. Signature files contribute nothing.
let extents (input: ParsedInput) : Extent list =
    match input with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        modules |> List.collect (fun (SynModuleOrNamespace(decls = decls)) -> List.collect moduleDecl decls)
    | ParsedInput.SigFile _ -> []
