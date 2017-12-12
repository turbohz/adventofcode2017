open System

let parsed c =
    match Int32.TryParse (string c) with
    | (true , i) -> Some i
    | (false, _) -> None

let compare v1 v2 = if v1 = v2 then Some v1 else None

let input = Console.ReadLine() |> List.ofSeq |> List.choose parsed
if input.Length % 2 > 0 then invalidArg "stdin" "Expecting a string of even length"

let output = 
    input 
    |> List.splitAt (input.Length / 2)  // split in half
    ||>List.map2 compare                // convert to a list of Option
    |> List.choose id                   // filter out None values 
    |> List.sum                         // sum values
    |> (*) 2                            // double it, because we only kept one of each pair

printfn "%d" output
exit 0