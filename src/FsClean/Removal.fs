/// Turns dead declarations into source edits.
///
/// Whole lines go, together with the comments directly above them, and the blank line that
/// separated the declaration from its neighbours goes with it. Anything the tool can't remove
/// cleanly it leaves in place and says why: what's left over is code a person should look at, and
/// the compiler is the backstop for the rest (see Fix).
module FsClean.Removal

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Compiler.Text
open FsClean.Analysis
open FsClean.Ast

type Outcome =
    {
      /// The new text of every file that changes.
      Changes: Map<string, string>
      Removed: Decl list
      Skipped: (Decl * string) list
    }

/// What gets deleted at once: one declaration, all of a chain that's dead, or a module that has
/// nothing left in it (which has no declaration of its own to report).
type private Deletion =
    { Decls: Decl list
      Range: range
      /// The module the deleted code is a whole declaration of, when it is one.
      Container: Container option }

let private rangeKey (r: range) =
    r.FileName, r.StartLine, r.StartColumn, r.EndLine, r.EndColumn

/// A chain that is entirely dead goes as one range. Otherwise each dead declaration after the
/// first goes on its own, from its `and`, but the first can't: its keyword would have to move to
/// the next declaration.
let private deletions (dead: Decl list) : Deletion list * (Decl * string) list =
    let alone, chained = dead |> List.partition (fun decl -> decl.Extent.Chain.IsNone)

    // A member is part of a type, not a declaration of the module around it.
    let container (decl: Decl) =
        if decl.Extent.Kind = Member then None else decl.Extent.Container

    let single =
        alone
        |> List.map (fun decl ->
            { Decls = [ decl ]
              Range = decl.Range
              Container = container decl })

    let byChain =
        chained
        |> List.groupBy (fun decl -> rangeKey decl.Extent.Chain.Value.Whole)
        |> List.map snd

    let whole, partial =
        byChain
        |> List.partition (fun group -> group.Length = group.Head.Extent.Chain.Value.Size)

    let wholeUnits =
        whole
        |> List.map (fun group ->
            { Decls = group
              Range = group.Head.Extent.Chain.Value.Whole
              Container = container group.Head })

    let partialUnits, skipped =
        partial
        |> List.collect id
        |> List.partition (fun decl -> decl.Extent.Chain.Value.Index > 0)

    let skipped =
        skipped
        |> List.map (fun decl ->
            decl, "it starts a chain (`let rec ... and`, `type ... and`) whose other members are live")

    // Taking one declaration out of a chain leaves the module's count of declarations alone.
    let partialDeletions =
        partialUnits
        |> List.map (fun decl ->
            { Decls = [ decl ]
              Range = decl.Range
              Container = None })

    single @ wholeUnits @ partialDeletions, skipped

/// A module can't be left with an empty body. When every declaration in one is being deleted, the
/// module goes too, and then the same goes for the module around it. A namespace can't go, so the
/// deletions that would empty one are called off.
let rec private withEmptiedModules (deletions: Deletion list) : Deletion list =
    let key (container: Container) = rangeKey container.Range

    let emptied =
        deletions
        |> List.choose (fun deletion -> deletion.Container)
        |> List.filter (fun container -> not container.IsNamespace)
        |> List.groupBy key
        |> List.filter (fun (_, group) -> group.Length = group.Head.Size)
        |> List.map (fun (_, group) -> group.Head)

    let known = deletions |> List.map (fun deletion -> rangeKey deletion.Range) |> Set.ofList

    match emptied |> List.filter (fun container -> not (known.Contains(key container))) with
    | [] -> deletions
    | modules ->
        modules
        |> List.map (fun container ->
            { Decls = []
              Range = container.Range
              Container = container.Outer })
        |> (@) deletions
        |> withEmptiedModules

let private isBlank (line: string) = String.IsNullOrWhiteSpace line

let private indentation (line: string) = line.Length - line.TrimStart().Length

/// `// ---- section ----` and the like head a group of declarations rather than describe one.
let private divider = Regex(@"^//+\s*[-=*#~_+]{3,}", RegexOptions.Compiled)

/// A comment directly above a declaration is about it: `///` documentation, or `//` lines that
/// aren't a section divider.
let private isAttachedComment (line: string) =
    let trimmed = line.Trim()

    trimmed.StartsWith "///"
    || (trimmed.StartsWith "//" && not (divider.IsMatch trimmed))

/// Line `i` (0-based) of the text, terminator included.
let private lineStarts (text: string) =
    [| yield 0
       for i in 0 .. text.Length - 1 do
           if text[i] = '\n' && i + 1 < text.Length then
               yield i + 1 |]

/// Deletes whole lines, plus the blank line that would otherwise be left doubled up. `None` when
/// the range shares its first or last line with code that stays.
let private deleteLines (text: string) (ranges: (Deletion * range) list) =
    let starts = lineStarts text
    let count = starts.Length

    let line i =
        text.Substring(starts[i], (if i + 1 < count then starts[i + 1] else text.Length) - starts[i])

    let content i = (line i).TrimEnd('\r', '\n')

    let deleted = HashSet<int>()
    let refused = ResizeArray<Deletion * string>()

    for deletion, range in ranges do
        let first = range.StartLine - 1
        let last = range.EndLine - 1
        let before = (content first).Substring(0, min range.StartColumn (content first).Length)
        let after = (content last).Substring(min range.EndColumn (content last).Length).Trim()

        if not (isBlank before) then
            refused.Add(deletion, "it shares its first line with other code")
        elif after <> "" && not (after.StartsWith "//") then
            refused.Add(deletion, "it shares its last line with other code")
        else
            // Comments directly above, with no blank line between, belong to the declaration.
            let mutable top = first

            while top > 0 && isAttachedComment (content (top - 1)) do
                top <- top - 1

            for i in top..last do
                deleted.Add i |> ignore

    // Removing a declaration between two blank lines would leave two in a row; at the end of the
    // file, a run of them.
    let mutable changed = true

    while changed do
        changed <- false
        let sorted = deleted |> Seq.sort |> Seq.toList

        for a in sorted |> List.filter (fun i -> not (deleted.Contains(i - 1))) do
            let mutable b = a

            while deleted.Contains(b + 1) do
                b <- b + 1

            let blankBefore = a = 0 || isBlank (line (a - 1))

            // The line above opens the block the removed code was the start of (`module X =`), so a
            // blank line after it would become a leading one.
            let opensBlock =
                a > 0 && not (isBlank (line (a - 1))) && indentation (line (a - 1)) < indentation (line a)

            if b + 1 < count then
                if (blankBefore || opensBlock) && isBlank (line (b + 1)) && deleted.Add(b + 1) then
                    changed <- true
            elif blankBefore then
                let mutable k = a - 1

                while k >= 0 && isBlank (line k) do
                    if deleted.Add k then
                        changed <- true

                    k <- k - 1

    let kept = [ for i in 0 .. count - 1 do if not (deleted.Contains i) then yield line i ]
    String.concat "" kept, Seq.toList refused

let plan (read: string -> string) (dead: Decl list) : Outcome =
    let blocked, removable = dead |> List.partition (fun decl -> decl.Blocker.IsSome)
    let deletions, chainSkips = deletions removable
    let deletions = withEmptiedModules deletions

    // The namespaces every declaration of which is going. Nothing in them may.
    let namespaces =
        deletions
        |> List.choose (fun deletion -> deletion.Container)
        |> List.filter (fun container -> container.IsNamespace)
        |> List.groupBy (fun container -> rangeKey container.Range)
        |> List.filter (fun (_, group) -> group.Length = group.Head.Size)
        |> List.map (fun (_, group) -> group.Head)

    let inEmptied (range: range) =
        namespaces |> List.exists (fun container -> Range.rangeContainsRange container.Range range)

    let deletions, namespaceSkips =
        let kept, refused = deletions |> List.partition (fun deletion -> not (inEmptied deletion.Range))

        let skipped =
            refused
            |> List.collect (fun deletion ->
                deletion.Decls
                |> List.map (fun decl ->
                    decl, "it would leave its namespace with no declarations, which the compiler treats as not defined, so `open` of it would fail"))

        kept, skipped

    let mutable changes = Map.empty
    let mutable removed = []
    let mutable skipped = []

    for file, fileDeletions in deletions |> List.groupBy (fun deletion -> deletion.Range.FileName) do
        let text, refused = deleteLines (read file) (fileDeletions |> List.map (fun deletion -> deletion, deletion.Range))
        let refusedDeletions = HashSet<Deletion>(refused |> List.map fst, HashIdentity.Reference)
        let applied = fileDeletions |> List.filter (fun deletion -> not (refusedDeletions.Contains deletion))

        if not applied.IsEmpty then
            changes <- Map.add file text changes

        removed <- removed @ (applied |> List.collect (fun deletion -> deletion.Decls))
        skipped <- skipped @ (refused |> List.collect (fun (deletion, reason) -> deletion.Decls |> List.map (fun decl -> decl, reason)))

    { Changes = changes
      Removed = removed
      Skipped = (blocked |> List.map (fun decl -> decl, decl.Blocker.Value)) @ chainSkips @ namespaceSkips @ skipped }
