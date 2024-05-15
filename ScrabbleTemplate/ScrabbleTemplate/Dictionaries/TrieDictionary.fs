module internal TrieDictionary  

    type TrieDict =
    | TrieNode of bool * Map<char, TrieDict>  

    let empty () = TrieNode (false, Map.empty)     
                                                    // Create an empty trie
    let insert (word: string) (trie: TrieDict) =                     
        let rec insertAux (chars: char list) (trieDict: TrieDict) =                      
            match chars with                       
            | [] -> 
                match trieDict with
                | TrieNode (_, children) -> TrieNode (true, children)                                                            // Mark the end of the word
            | c::cs ->
                match trieDict with
                | TrieNode (isWord, children) -> 
                    match Map.tryFind c children with
                    | Some existingChild -> TrieNode (isWord, children.Add(c, (insertAux cs existingChild)))              // Recurse down the trie
                    | None               -> TrieNode (isWord, children.Add(c, (insertAux cs (empty ()))))                 // Add a new node to the trie
        insertAux [for char in word -> char] trie

    let lookup (word: string) (trie: TrieDict) =
        let rec lookupAux (chars: char list) (trieDict: TrieDict) =
            match chars with
            | [] -> 
                match trieDict with
                | TrieNode (isWord, _) -> isWord
            | c::cs ->
                match trieDict with
                | TrieNode (_, children) -> 
                    match Map.tryFind c children with
                    | Some child -> lookupAux cs child
                    | None -> false
        lookupAux [for char in word -> char] trie

    let step (char: char) (trie: TrieDict) : (bool * TrieDict) option =
        match trie with
        | TrieNode (_, children) -> 
            match children.TryFind(char) with
            | Some (TrieNode(isWord, _children)) -> Some (isWord, TrieNode(isWord, _children))
            | None -> None