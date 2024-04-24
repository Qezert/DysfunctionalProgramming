module GADDAG =

    open System.Collections.Generic

    type GADDAGNode = 
        | EmptyNode
        | EndOfWord of bool
        | LetterNode of char * GADDAGNode
        | HookNode of char * GADDAGNode * GADDAGNode

    type GADDAGDictionary() =
        let rootNode = new Dictionary<char, GADDAGNode>()

        // Helper function to insert a word into the GADDAG
        let rec insertWord (word: string) (node: GADDAGNode) =
            match word with
            | "" -> node
            | _ -> 
                let firstChar = word.[0]
                let remainder = word.Substring(1)
                match node with
                | EmptyNode -> 
                    let newNode = insertWord remainder (LetterNode(firstChar, EmptyNode))
                    rootNode.Add(firstChar, newNode)
                    newNode
                | EndOfWord isEnd -> 
                    if remainder = "" then
                        EndOfWord true
                    else
                        let newNode = insertWord remainder (LetterNode(firstChar, EmptyNode))
                        rootNode.Add(firstChar, newNode)
                        newNode
                | LetterNode(c, child) -> 
                    if remainder = "" then
                        LetterNode(c, insertWord remainder child)
                    else
                        let newNode = insertWord remainder child
                        rootNode.Add(c, newNode)
                        LetterNode(c, newNode)
                | HookNode(c, left, right) -> 
                    if remainder = "" then
                        HookNode(c, left, insertWord remainder right)
                    else
                        let newNode = insertWord remainder right
                        rootNode.Add(c, newNode)
                        HookNode(c, left, newNode)

        // Public method to insert a word into the GADDAG
        member this.InsertWord (word: string) =
            let reversedWord = new string(Array.rev <| word.ToCharArray())
            let _ = insertWord ("$" + reversedWord) EmptyNode

        // Public method to check if a word is in the dictionary
        member this.Contains (word: string) =
            let rec searchNode (str: string) (node: GADDAGNode) =
                match str, node with
                | "", EndOfWord isEnd -> isEnd
                | "", _ -> false
                | _, EmptyNode -> false
                | _, LetterNode(c, child) ->
                    let rest = str.Substring(1)
                    if c = str.[0] then
                        searchNode rest child
                    else
                        false
                | _, HookNode(c, left, right) ->
                    if str.[0] = c then
                        let rest = str.Substring(1)
                        searchNode rest right
                    else
                        searchNode str left
            
            let reversedWord = new string(Array.rev <| word.ToCharArray())
            let result = searchNode ("$" + reversedWord) EmptyNode
            result

// Usage example:
let dict = new GADDAGDictionary()
dict.InsertWord "hello"
dict.InsertWord "world"
printfn "%b" (dict.Contains "hello") // true
printfn "%b" (dict.Contains "world") // true
printfn "%b" (dict.Contains "foo")   // false
