#load "util.fs"

open System
open Util.Stdin
let parsed c =
    match Int32.TryParse (string c) with
    | (true , i) -> Some i
    | (false, _) -> None
let computeLineChecksum cells = (Seq.max cells - Seq.min cells)
let cells = Util.Stdin.lines |> Seq.map (fun l -> l.Split('\t') |> Seq.choose parsed)
let output = cells |> Seq.sumBy computeLineChecksum

printfn "%d" output
exit 0