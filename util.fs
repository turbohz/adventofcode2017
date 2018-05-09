namespace Util

open System

module Stdin =

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

type Parser = 
    Parser with
        static member maybeInt (c:char)   : int option = Parser.maybeInt (string c)
        static member maybeInt (s:string) : int option = 
            match Int32.TryParse s with
            | (true , i) -> Some i
            | (false, _) -> None
