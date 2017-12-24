#load "util.fs"

open System
open Util.Stdin

let isValidPassphrase (p:string) : bool =
    let allWords = p.Split(' ') |> List.ofSeq
    let distinctWords = List.distinct allWords
    List.length allWords = List.length distinctWords && List.length distinctWords >= 2
let result = Util.Stdin.lines |> Seq.filter isValidPassphrase |> Seq.length

printfn "%i" result
exit 0