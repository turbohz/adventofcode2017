#load "util.fs"

open System
open Util.Stdin

// Two words are anagrams if they have the same letters in the same amount
// anagramHash will return the same value for two such words
let anagramHash (s:string) = 
    let letterCount = s.ToCharArray() |> Array.countBy id |> Array.sort
    hash letterCount

let isValidPassphrase (p:string) : bool =
    let allWords = p.Split(' ') |> List.ofSeq
    let distinctWords = allWords |> List.map anagramHash |> List.distinct 
    List.length allWords = List.length distinctWords && List.length distinctWords >= 2
let result = Util.Stdin.lines |> Seq.filter isValidPassphrase |> Seq.length

printfn "%i" result
exit 0