#load "util.fs"

open System
open Util.Stdin
let parsed c =
    match Int32.TryParse (string c) with
    | (true , i) -> Some i
    | (false, _) -> None
let mutable jumptable = Util.Stdin.lines |> Seq.choose parsed |> Array.ofSeq
let (|OutOfBounds|_|) (jumptable:int []) (pointer:int) = 
    if (pointer < 0 || pointer >= jumptable.Length) then Some pointer else None
let rec execute (stepcount:int) (pointer:int) (jumptable:int [] byref) =
    match pointer with
    | OutOfBounds jumptable _ -> stepcount
    | _ ->
        let offset = Array.get jumptable pointer
        let destination = pointer + offset
        Array.set jumptable pointer (offset+1) |> ignore
        execute (stepcount + 1) (pointer + offset) &jumptable

let output = execute 0 0 &jumptable

printfn "%i" output
exit 0
