#load "util.fs"

open System
open Util.Stdin
let parsed c =
    match Int32.TryParse (string c) with
    | (true , i) -> Some i
    | (false, _) -> None
let evenlyDividable x1 x2 = (x1 % x2 = 0)
let rec computeLineChecksum =
    function
    | [] -> failwith "Did not find two evenly divisible elements. Doh!"
    | x1::xs ->
        let maybeEvenDivision = xs |> List.tryFind (evenlyDividable x1) |> Option.map (x1 |> (/))
        match maybeEvenDivision with
        | Some v -> v
        | None -> computeLineChecksum xs

let cells = Util.Stdin.lines |> Seq.map (fun l -> l.Split('\t') |> Seq.choose parsed |> Seq.sortDescending |> List.ofSeq)
let output = cells |> Seq.sumBy computeLineChecksum

printfn "%d" output
exit 0