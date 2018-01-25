open System

let parsed s =
    match Int32.TryParse s with
    | (true , i) -> Some i
    | (false, _) -> None
let input = Console.ReadLine().Split('\t') |> Seq.choose parsed |> Seq.toList

module Memory =
    [<StructuredFormatDisplay("{A}")>]
    type M = Memory of Map<int,int> with 
        override this.ToString() = 
            let (Memory m) = this
            sprintf "Memory: %s\n" ( m |> Map.toSeq |> Seq.map (fun (_,v) -> " " + (string v)) |> Seq.fold (+) "")
        member this.A = this.ToString()
    let ofList l =
        Memory (l |> List.mapi (fun i v -> (i,v)) |> Map.ofList)
    let fullest (Memory m) = 
        if Map.isEmpty m then 
            failwith "Memory has no defined banks. Can't find fullest of nothing."
        else 
            let fullestBank = m |> Map.fold (fun (maxk, maxv) k v -> if v > maxv then (k,v) else (maxk,maxv)) (0, m.[0])
            fst fullestBank
    let allocate k (Memory m) =
        match (Map.tryFind k m) with
        | None -> failwith (sprintf "Invalid Memory Bank:%i"  k)
        | Some v -> Memory (m |> Map.add k (v+1))
    let empty k (Memory m) =
       match (Map.tryFind k m) with
       | None -> failwith (sprintf "Invalid Memory Bank:%i"  k)
       | Some _ -> Memory (m |> Map.add k 0)
    let read k (Memory m) =
        match (Map.tryFind k m) with
        | None -> failwith (sprintf "Invalid Memory Bank:%i"  k)
        | Some v -> v
    let banks (Memory m) = 
        Map.count m 

let reallocate m = 
    let from = Memory.fullest m
    let blocks = Memory.read from m
    // empy fullest block first
    let deallocated = Memory.empty from m
    // distribute blocks
    let rec distribute k b m =
        match b with
        | 0 -> m
        | _ ->
            let k' = (k + 1) % (Memory.banks m)
            let m' = Memory.allocate k' m
            let b' = b - 1
            distribute k' b' m'
    
    distribute from blocks deallocated

type SeenStates = Set<Memory.M>
let mutable history:SeenStates = Set.empty
let mutable memory:Memory.M = Memory.ofList input

while not (Set.contains memory history) do
    history <- history |> Set.add memory 
    memory <- memory |> reallocate

printf "%i" (history |> Set.count) 
exit 0
