module Trie =
    type Trie = 
        | Node of bool * Map<char, Trie>
        | Empty

    let empty = Empty

    let rec insert (word : string) (trie : Trie) =
        match word with
        | "" -> Node(true, Map.empty)
        | _ -> 
            let (c, cs) = (word.[0], word.Substring(1))
            match trie with
            | Empty -> Node(false, Map.singleton c (insert cs Empty))
            | Node(isWord, children) -> 
                if Map.containsKey c children then
                    Node(isWord, Map.add c (insert cs (Map.find c children)) children)
                else
                    Node(isWord, Map.add c (insert cs Empty) children)

    let rec contains (word : string) (trie : Trie) =
        match word with
        | "" -> true
        | _ -> 
            let (c, cs) = (word.[0], word.Substring(1))
            match trie with
            | Empty -> false
            | Node(isWord, children) -> 
                if Map.containsKey c children then
                    contains cs (Map.find c children)
                else
                    false

    let rec step (c : char) (trie : Trie) =
        match trie with
        | Empty -> Empty
        | Node(isWord, children) -> 
            if Map.containsKey c children then
                Map.find c children
            else
                Empty

    let rec reverse (trie : Trie) =
        let rec aux (trie : Trie) (word : string) (words : string list) =
            match trie with
            | Empty -> words
            | Node(isWord, children) -> 
                let newWord = if isWord then word else ""
                let newWords = if isWord then newWord::words else words
                children
                |> Map.fold (fun c t acc -> aux t (word + string c) acc) newWords
        aux trie "" []