/// Unified diffs of files that were edited only by deleting lines, which is all Removal does.
module FsClean.Diff

let private context = 3

/// Lines keep their `\r`, so that the diff applies to a file with Windows line endings.
let private lines (text: string) =
    let parts = text.Split '\n'

    // A final newline doesn't start another line.
    if text.EndsWith "\n" then Array.take (parts.Length - 1) parts else parts

/// Which of the original's lines are missing from the edited text, or None if the edited text isn't
/// the original with lines deleted.
let private deletedLines (original: string[]) (edited: string[]) =
    let deleted = ResizeArray<int>()
    let mutable next = 0

    for i in 0 .. original.Length - 1 do
        if next < edited.Length && original[i] = edited[next] then
            next <- next + 1
        else
            deleted.Add i

    if next = edited.Length then Some(Seq.toList deleted) else None

/// The diff of `path` from `before` to `after`, one line per element; empty if nothing differs.
let unified (path: string) (before: string) (after: string) : string list =
    let original = lines before

    match deletedLines original (lines after) with
    | Some [] -> []
    | None -> [ $"--- a/{path}"; $"+++ b/{path}"; "(not shown: the edit was not just deleted lines)" ]
    | Some deleted ->
        let deleted = List.toArray deleted
        let isDeleted = System.Collections.Generic.HashSet<int>(deleted)
        let mutable position = 0

        let hunks =
            [ while position < deleted.Length do
                  let start = max 0 (deleted[position] - context)
                  let mutable last = deleted[position]

                  // Deletions close enough together share a hunk.
                  while position + 1 < deleted.Length && deleted[position + 1] - last <= 2 * context + 1 do
                      position <- position + 1
                      last <- deleted[position]

                  position <- position + 1
                  let stop = min (original.Length - 1) (last + context)
                  let removed = [ start..stop ] |> List.filter isDeleted.Contains |> List.length
                  let removedBefore = deleted |> Array.filter (fun i -> i < start) |> Array.length
                  let oldCount = stop - start + 1
                  let newCount = oldCount - removed
                  let keptBefore = start - removedBefore
                  let newStart = if newCount = 0 then keptBefore else keptBefore + 1

                  yield $"@@ -{start + 1},{oldCount} +{newStart},{newCount} @@"

                  for i in start..stop do
                      yield (if isDeleted.Contains i then "-" else " ") + original[i] ]

        $"--- a/{path}" :: $"+++ b/{path}" :: hunks
