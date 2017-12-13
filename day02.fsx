open System

let parsed c =
    match Int32.TryParse (string c) with
    | (true , i) -> Some i
    | (false, _) -> None

let (|Line|_|) (r:IO.TextReader) = if r.Peek() = -1 then None else Some (r.ReadLine())
let rec readLines (reader:IO.TextReader) (lines: string list) : string list = 
    match reader with
    | Line l -> readLines reader (lines @ [l])
    | _ -> lines

let computeLineChecksum cells = (List.max cells - List.min cells)

let input = Console.OpenStandardInput()
let reader = new IO.StreamReader(input)
// create a list of lists of ints
let cells = [] |> readLines reader |> List.map (fun l -> l.Split('\t') |> List.ofSeq |> List.choose parsed)
let output = cells |> List.sumBy computeLineChecksum

printfn "%d" output
exit 0