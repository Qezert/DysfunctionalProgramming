module internal Dictionary

    type TrieDictionary
        val empty : TrieDictionary
        val add : string -> TrieDictionary -> TrieDictionary
        val contains : string -> TrieDictionary -> bool
        val next : char -> TrieDictionary -> TrieDictionary