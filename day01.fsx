open System

let parsed c =
    match Int32.TryParse (string c) with
    | (true , i) -> Some i
    | (false, _) -> None

let firstValueIfEqualToLast l =
    let first = l |> List.head
    let last = l |> List.rev |> List.head
    if first = last then first else 0

let rec collectSumOfEqualPairs s l =
    match l with
    | [] -> 0
    | [_] -> s
    | x1::x2::xs -> 
        let v = if x1=x2 then x1 else 0
        collectSumOfEqualPairs (s+v) (x2::xs)

let input = Console.ReadLine() |> List.ofSeq |> List.choose parsed

// We apply a divide an conquer approach:
// - collect the sum of equal pairs
// - add to that the first value if equal to the last value
let output = (collectSumOfEqualPairs 0 input) + (firstValueIfEqualToLast input)
printfn "%d" output
exit 0