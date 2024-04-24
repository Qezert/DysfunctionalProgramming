module MultiSet

    open System.ComponentModel

    type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32>

    let empty = M Map.empty

    let isEmpty (M s) = Map.isEmpty s

    let size (M s) = Map.fold (fun acc _ v -> acc + v) 0u s
    
    let contains (a: 'a) (M s) = Map.containsKey a s

    let numItems (a : 'a) (M s) =
        match Map.tryFind a s with
        | Some b -> b
        | None -> 0u

    let add (a : 'a) (n : uint32) (M s) : MultiSet<'a> =
        match s.TryFind a with
            | Some b -> M (s.Add(a, n + b))
            | None -> M (s.Add(a, n))

    let addSingle (a : 'a) (M s) : MultiSet<'a> =
        add a 1u (M s)
    
    let remove (a : 'a) (n : uint32) (M s) : MultiSet<'a> =
        match s.TryFind a with
            | Some b -> if b > n then M (s.Add(a, b - n)) else M (s.Remove a)
            | None -> M s

    let removeSingle (a : 'a) (M s) : MultiSet<'a> =
        remove a 1u (M s)

    let fold f acc (M s) = Map.fold f acc s
    let foldBack f (M s) acc = Map.foldBack f s acc
    

    let ofList (_ : 'a list) : MultiSet<'a> = M Map.empty
    let toList (_ : MultiSet<'a>) : 'a list = []

    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = M Map.empty

    let union (a : MultiSet<'a>) (b : MultiSet<'a>) : MultiSet<'a> =
        let f (k : 'a) (v : uint32) (acc : MultiSet<'a>) =
            add k v acc
        foldBack f a b
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = M Map.empty

    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = M Map.empty

    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = M Map.empty