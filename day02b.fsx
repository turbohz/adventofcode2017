open System
open System.Xml.Xsl

let parsed c =
    match Int32.TryParse (string c) with
    | (true , i) -> Some i
    | (false, _) -> None

let (|Line|_|) (r:IO.TextReader) = if r.Peek() = -1 then None else Some (r.ReadLine())
let rec readLines (reader:IO.TextReader) (lines: string list) : string list = 
    match reader with
    | Line l -> readLines reader (lines @ [l])
    | _ -> lines

let evenlyDividable x1 x2 = (x1 % x2 = 0)

let rec computeLineChecksum =
    function
    | [] -> failwith "Did not find two evenly divisible elements. Doh!"
    | x1::xs ->
        let maybeEvenDivision = xs |> List.tryFind (evenlyDividable x1) |> Option.map (x1 |> (/))
        match maybeEvenDivision with
        | Some v -> v
        | None -> computeLineChecksum xs

let input = Console.OpenStandardInput()
let reader = new IO.StreamReader(input)
// create a list of lists of ints
let cells = [] |> readLines reader |> List.map (fun l -> l.Split('\t') |> List.ofSeq |> List.choose parsed |> List.sortDescending)
let output = cells |> List.sumBy computeLineChecksum

printfn "%d" output
exit 0