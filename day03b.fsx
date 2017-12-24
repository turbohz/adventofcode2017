open System
open FSharp.Collections

type Coordinate = int * int
type Memory = Map<Coordinate, int>
type MemoryIndexEnumerator = Collections.Generic.IEnumerator<Coordinate>

type Direction = 
    | Right of int
    | Up of int
    | Left of int
    | Down of int


// This was not the simplest Seq practice!
let rec spiralSeq (c:Coordinate) (d:Direction) : Coordinate seq = 
    let inline next (x,y) d = 
        match d with
        | Right n -> seq{for x' in x+1 .. x+n -> x',y}      , Up n
        | Up    n -> seq{for y' in y+1 .. y+n -> x,y'}      , Left (n+1)
        | Left  n -> seq{for x' in x-1 .. -1 .. x-n -> x',y}, Down n
        | Down  n -> seq{for y' in y-1 .. -1 .. y-n -> x,y'}, Right (n+1)

    seq {
        let cs, d' = next c d
        yield! cs // current side
        yield! spiralSeq (Seq.last cs) d' // next side
    }

let spiral (origin:Coordinate) : Coordinate seq = seq { yield! spiralSeq origin (Right 1) }
let squareAt ((x,y):Coordinate) : Coordinate seq = 
    [ (x+1,y  )
    ; (x+1,y+1)
    ; (x  ,y+1)
    ; (x-1,y+1)
    ; (x-1,y  )
    ; (x-1,y-1)
    ; (x  ,y-1)
    ; (x+1,y-1)
    ] |> Seq.ofList

let computeValueAt (c:Coordinate) (m:Memory) : int =
    (squareAt c) |> Seq.choose (fun c -> Map.tryFind c m) |> Seq.sum

let input = Console.ReadLine() |> int
let addressGenerator = (spiral (0,0)).GetEnumerator()
let mutable memory : Memory = [((0,0),1)] |> Map.ofList

let rec allocateWhileValue (condition:int->bool) (addresses:MemoryIndexEnumerator) (memory:Memory byref) =
    addresses.MoveNext() |> ignore
    let nextAddr = addresses.Current
    let nextVal = computeValueAt nextAddr memory
    let wedone = condition nextVal
    if wedone 
        then 
            nextVal 
        else 
            memory <- memory.Add(nextAddr,nextVal)
            allocateWhileValue condition addresses &memory
let output = allocateWhileValue ((<) input) addressGenerator &memory

printfn "%A" output
exit 0
