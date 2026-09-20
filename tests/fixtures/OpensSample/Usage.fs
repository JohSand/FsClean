module Sample.Usage

open System.Text
open System.IO
open System.Collections.Generic
open Sample.Builders
open Sample.BuilderExtensions

let shout (text: string) = StringBuilder(text).Append("!").ToString()

let printAll () =
    build {
        for item in Box [ 1; 2; 3 ] do
            printfn "%d" item
    }
