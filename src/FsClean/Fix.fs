/// Applies removals to the files on disk, keeping only the ones the compiler accepts. A preview
/// does everything but write: the compiler is shown the edited files from memory.
///
/// Whatever the analysis says, a removal only stays if the projects still type-check without it.
/// A failing batch is split in half and each half tried again, so one wrong finding costs that
/// finding rather than the whole run. Dead declarations that use each other are never split apart:
/// removing a helper without its dead caller can't compile, and says nothing about the helper.
module FsClean.Fix

open System.Collections.Generic
open System.IO
open System.Runtime.ExceptionServices
open System.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.IO
open FsClean.Analysis
open FsClean.ProjectLoader

type Mode =
    /// Write the removals to the files.
    | Apply
    /// Work out and check the removals, and change nothing.
    | Preview

type Result =
    {
      Removed: Decl list
      /// Left in place, and why.
      Kept: (Decl * string) list
      Files: string list
      /// Each changed file with its text before and after.
      Edited: (string * string * string) list
    }

let private utf8Bom = [| 0xEFuy; 0xBBuy; 0xBFuy |]

let private hasBom (bytes: byte[]) =
    bytes.Length >= 3 && bytes[0] = utf8Bom[0] && bytes[1] = utf8Bom[1] && bytes[2] = utf8Bom[2]

let private isUtf16 (bytes: byte[]) =
    bytes.Length >= 2
    && ((bytes[0] = 0xFFuy && bytes[1] = 0xFEuy) || (bytes[0] = 0xFEuy && bytes[1] = 0xFFuy))

let private decode (bytes: byte[]) =
    let start = if hasBom bytes then 3 else 0
    Encoding.UTF8.GetString(bytes, start, bytes.Length - start)

/// Keeps the byte order mark the file came with.
let private encode (original: byte[]) (text: string) =
    let body = UTF8Encoding(false).GetBytes text

    if hasBom original then
        Array.append utf8Bom body
    else
        body

/// Serves some files' contents from memory, so the compiler can be asked about edits that were
/// never written.
type private Overlay(files: IReadOnlyDictionary<string, byte[]>) =
    inherit DefaultFileSystem()

    override _.OpenFileForReadShim(filePath, ?useMemoryMappedFile, ?shouldShadowCopy) : Stream =
        match files.TryGetValue filePath with
        | true, bytes -> new MemoryStream(bytes) :> Stream
        | _ -> base.OpenFileForReadShim(filePath, ?useMemoryMappedFile = useMemoryMappedFile, ?shouldShadowCopy = shouldShadowCopy)

/// The compiler's file system is one setting for the whole process, so previews take turns.
let private previewLock = obj ()

let run (mode: Mode) (projects: Project list) (dead: Decl list) : Async<Result> =
    async {
        let originals =
            dead
            |> List.map (fun decl -> decl.Range.FileName)
            |> List.distinct
            |> List.map (fun file -> file, File.ReadAllBytes file)
            |> Map.ofList

        // Only UTF-8 is handled; a file in anything else is left alone rather than mangled.
        let unsupported, editable =
            dead |> List.partition (fun decl -> isUtf16 originals[decl.Range.FileName])

        // What each file should currently contain; in a preview, what it would.
        let current = Dictionary<string, byte[]>()
        originals |> Map.iter (fun file bytes -> current[file] <- bytes)

        let read file = decode originals[file]

        /// Makes the files what removing `decls` gives, and every other file what it was.
        let apply (decls: Decl list) =
            let outcome = Removal.plan read decls

            for KeyValue(file, original) in originals do
                let wanted =
                    match Map.tryFind file outcome.Changes with
                    | Some text -> encode original text
                    | None -> original

                if wanted <> current[file] then
                    if mode = Apply then
                        File.WriteAllBytes(file, wanted)

                    current[file] <- wanted

            outcome

        let check (decls: Decl list) =
            async {
                apply decls |> ignore

                match mode with
                | Apply -> return! typeErrors (FSharpChecker.Create()) projects
                | Preview ->
                    return
                        lock previewLock (fun () ->
                            let real = FileSystemAutoOpens.FileSystem
                            FileSystemAutoOpens.FileSystem <- Overlay(Dictionary<string, byte[]>(current))

                            try
                                typeErrors (FSharpChecker.Create()) projects |> Async.RunSynchronously
                            finally
                                FileSystemAutoOpens.FileSystem <- real)
            }

        let rec solve (accepted: Decl list) (units: Decl list list) (rejected: (Decl list * string) list) =
            async {
                if List.isEmpty units then
                    return accepted, rejected
                else
                    let candidate = accepted @ List.concat units
                    let! errors = check candidate

                    if List.isEmpty errors then
                        return candidate, rejected
                    else
                        match units with
                        | [ single ] ->
                            let why = $"removing it doesn't type-check: {List.head errors}"
                            return accepted, (single, why) :: rejected
                        | _ ->
                            let left, right = List.splitAt (units.Length / 2) units
                            let! accepted, rejected = solve accepted left rejected
                            return! solve accepted right rejected
            }

        try
            let units =
                editable
                |> List.groupBy (fun decl -> decl.Component)
                |> List.sortBy fst
                |> List.map snd

            let! accepted, rejected = solve [] units []
            // Leave the files as the accepted set alone makes them, whatever was tried last.
            let outcome = apply accepted

            return
                { Removed = outcome.Removed
                  Kept =
                    (unsupported |> List.map (fun decl -> decl, "its file isn't UTF-8"))
                    @ outcome.Skipped
                    @ (rejected |> List.collect (fun (decls, why) -> decls |> List.map (fun decl -> decl, why)))
                  Files = outcome.Changes |> Map.toList |> List.map fst
                  Edited =
                    outcome.Changes
                    |> Map.toList
                    |> List.map (fun (file, text) ->
                        // The byte order mark is part of the file's first line.
                        let mark = if hasBom originals[file] then "\uFEFF" else ""
                        file, mark + read file, mark + text) }
        with error ->
            // Put every file back before letting the failure through.
            apply [] |> ignore
            ExceptionDispatchInfo.Capture(error).Throw()
            return Unchecked.defaultof<Result>
    }
