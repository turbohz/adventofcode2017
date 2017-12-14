open System


(*

The approach is to move the complexity
to the "spiral" structure itseld, and 
make easy to compute the Manhattan distance.

To do so, the structure as a list
where the index is the value in the grid
and the value is a tuple of the coordinate

Since 1 is at (0,0), the distance can be
easily computed by doing (abs x + abs y)

So: 
    [ ( 0, 0) // 1
    ; ( 1, 0) // 2
    ; ( 1, 1) // 3
    ; ( 0, 1) // 4
    ; (-1, 1) // 5
    ; (-1, 0) // 6
    ; (-1,-1) // 7
    ; ( 0,-1) // 8
    ; ( 1,-1) // 9
    ; ( 2,-1) // 10
    ]

*)

type Direction = 
    | Right of int
    | Up of int
    | Left of int
    | Down of int

type Coordinate = int * int

// This was not the simplest Seq practice!
let rec spiralSeq (c:Coordinate) (d:Direction) : seq<Coordinate> = 
    let next (x,y) d = 
        match d with
        | Right n -> 
            (x+n,y),
            seq{for x' in x+1 .. x+n -> x',y}, 
            Up n
        | Up    n -> 
            (x,y+n),
            seq{for y' in y+1 .. y+n -> x,y'}, 
            Left (n+1)
        | Left  n -> 
            (x-n,y),
            seq{for x' in x-1 .. -1 .. x-n -> x',y}, 
            Down n
        | Down  n -> 
            (x,y-n),
            seq{for y' in y-1 .. -1 .. y-n -> x,y'}, 
            Right (n+1)

    seq {
        let c', cs, d' = next c d
        yield! cs // current side
        yield! spiralSeq c' d' // next side
    }

let spiral n = seq { yield (0,0); yield! Seq.take (n-1) (spiralSeq (0,0) (Right 1)) }

let manhattanDistance n =
    let x,y = (spiral n |> Seq.item (n-1))
    abs x + abs y

let input = Console.ReadLine()
let output = input |> int |> manhattanDistance 

printfn "%i" output
exit 0
