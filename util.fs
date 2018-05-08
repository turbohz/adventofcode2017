namespace Util

module Stdin =

    open System

    let internal (|Line|_|) (r:IO.TextReader) = if r.Peek() = -1 then None else Some (r.ReadLine())
    let lines : string seq =

        let input = Console.OpenStandardInput()
        let reader = new IO.StreamReader(input)

        let read reader = 
            match reader with
            | Line l -> Some(l,reader)
            | _ -> 
                reader.Dispose()
                input.Dispose()
                None

        Seq.unfold read reader

module Parser =
    open System
    let maybeInt (x:'T) : int option =
        box x
        |> function
            | :? char as c   -> string c
            | :? string as s -> s
            | _ -> "Unexpected" // reject anything else
        |> Int32.TryParse        
        |> function
            | (true , i) -> Some i
            | (false, _) -> None
