module MultiSet

    type MultiSet<'a when 'a : comparison> = S of Map<'a, uint32> // replace with your type

    // green exercises
    let empty = 
        S Map.empty

    let isEmpty (S set : MultiSet<'a>) = 
        Map.isEmpty set

    let size (S set) =
        set
        |> Map.fold (fun acc _ count -> acc + uint32(count)) 0u
    
    let contains (key : 'a) (S set : MultiSet<'a>) = 
        Map.containsKey key set

    let numItems (key : 'a) (S set : MultiSet<'a>) = 
        match Map.tryFind key set with
        | None -> 0u
        | Some count -> count

    let add (key : 'a) (count : uint32) (S set : MultiSet<'a>) : MultiSet<'a> = 
        let total = count + (numItems key (S set))
        S (Map.add key total set)

    let addSingle (key : 'a) (S set : MultiSet<'a>) : MultiSet<'a> = 
        add key 1u (S set)
    
    let remove (key : 'a) (amount : uint32) (S set : MultiSet<'a>) : MultiSet<'a> = 
        let count = numItems key (S set)
        if count > amount
            then
                S ((Map.remove key set).Add(key, (count - amount))) //removes the key then adds the key again with remaining amount
            else
                S (set.Remove key)

    let removeSingle (key : 'a) (S set : MultiSet<'a>) : MultiSet<'a> = 
        let count = numItems key (S set)
        remove key 1u (S set)

    let fold (f : 'b -> 'a -> uint32 -> 'b) (x : 'b) (S set : MultiSet<'a>) = 
        Map.fold f x set 
    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (S set : MultiSet<'a>) (x : 'b) = 
        Map.foldBack f set x
    
    // yellow exercises
    let ofList (_ : 'a list) : MultiSet<'a> = failwith "not done"
    let toList (_ : MultiSet<'a>) : 'a list = failwith "not done"


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = failwith "not done"

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not done"
       
    