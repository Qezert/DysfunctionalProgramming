module internal Dictionary

    type TrieDictionary
    | TrieNode of bool * Map<char, TrieDictionary>

    let empty = TrieNode (false, Map.empty)                             // Create an empty trie

    let add (word: string) (trie: TrieDictionary) =
        let rec addAux (word: string) (node: TrieDictionary) =
            match word, node with
            | "" -> TrieNode (_, children) -> TrieNode (true, children) // Mark the end of the word
            | c::cs -> TrieNode (isWord, children) ->                   // Add the character to the trie
                let child =                                             // Get the child node for the character
                match Map.tryFind c children with                       // Check if the child already exists
                    | Some childNode -> childNode                       // If the child already exists, return it
                    | None -> empty                                     // Create a new child if it doesn't exist 
                let newChild = addAux cs child                          // Recursively add the rest of the word
                TrieNode (isWord, children.Add(c newChild))             // Add the new child to the children

        addAux word trie

    let contains (word: string) (trie: TrieDictionary) =                // Check if the trie contains a word
        let rec containsAux (word: string) (node: TrieDictionary) =     // Recursive helper function
            match word, node with                                       // Match on the word and the node
            | "", TrieNode (isWord, _) -> isWord                        // If the word is empty, return whether the node is a word
            | c::cs, TrieNode (_, children) ->                          // If the word is not empty, check the children
                match Map.tryFind c children with                       // Check if the character is in the children
                | Some childNode -> containsAux cs childNode            // If the character is in the children, recurse on the child
                | None -> false                                         // If the character is not in the children, return false

        containsAux word trie

    let findFirstWord (chars: char list) (trie: TrieDictionary) =                           // Find the first word in the trie that can be made with the given characters
    let rec dfsAux (node: TrieDictionary) (prefix: string) (charsLeft: Map<char, int>) =    // Depth-first search helper function
        match node with                                                                     // Match on the node                             
        | TrieNode (true, _) -> Some prefix                                                 // If the node is a word, return the prefix
        | TrieNode (_, children) ->                                                         // If the node is not a word, check the children
            match List.tryPick (fun (c, child) ->                                           // Try to find a child that can be used                 
                match Map.tryFind c charsLeft with                                          // Check if the character is in the characters left      
                | Some count when count > 0 ->                                              // If the character is in the characters left     
                    let updatedChars = charsLeft |> Map.add c (count - 1)                   // Update the characters left     
                    dfsAux child (prefix + string c) updatedChars                           // Recurse on the child with the updated characters left          
                | _ -> None) children with                                                  // If the character is not in the characters left, return None        
            | Some result -> result                                                         // If a child was found, return the result              
            | None -> None                                                                  // If no child was found, return None         