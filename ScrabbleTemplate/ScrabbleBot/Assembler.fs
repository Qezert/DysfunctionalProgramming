module internal Assembler

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open State
    open System
    open MultiSet
    open StateMonad
    open System.Text
    open System.Threading

    type Direction =
        | Horizontal
        | Vertical
        | Center

    type Operator =
        | Add
        | Subtract

    let charValues (char: char) =
        match System.Char.ToUpper(char) with
        | 'A' | 'E' | 'I' | 'O' | 'U' | 'L' | 'N' | 'S' | 'T' | 'R'  -> 1
        | 'D' | 'G' -> 2
        | 'B' | 'C' | 'M' | 'P'  -> 3
        | 'F' | 'H' | 'V' | 'W' | 'Y' -> 4
        | 'K' -> 5
        | 'J' | 'X' -> 8
        | 'Q' | 'Z' -> 10
        | _ -> 0

    let isFreshBoard st =
        (st.piecesOnBoard |> Map.isEmpty)

    let uint32ToChar c =
        char(c + 64u)

    let uint32ToInt x =
        int(x)

    let charToUint32 c =
        uint32(System.Char.ToUpper(c)) - 64u

    let doesTileExist (st : state) (c : coord) =
        match (st.board.squares c) with
        | Success _ -> true
        | Failure _ -> false

    let isPieceOnTile st coord =
        st.piecesOnBoard |> Map.containsKey coord

    let uint32MSToCharMS = 
        map (fun msType -> uint32ToChar msType)

    let getCharsInHand st =
        st.hand 
        |> MultiSet.fold (fun charsSet x i ->                                                                   // Fold over the multiset
            let char = uint32ToChar(x)                                                                          // Convert the uint32 to a char
            Seq.append charsSet (Seq.init (int i) (fun _ -> char))) Seq.empty         // Append the char to the set i times
        |> Seq.toList  

    let removeCharFromList (char: char) (chars: char list) : char list =
        List.filter (fun c -> c <> char) chars

    let printCharsInHand st : string =
        let chars = getCharsInHand st
        let charsStr = String.Join(", ", chars)
        charsStr

    let mergeCoords (coord1: coord) (coord2: coord) (op: Operator) =
        let (x1, y1) = coord1
        let (x2, y2) = coord2
        match op with
        | Add       -> (x1 + x2, y1 + y2)
        | Subtract  -> (x1 - x2, y1 - y2)

    let dirToCoord direction =
        match direction with
        | Horizontal    -> (1, 0)
        | Vertical      -> (0, 1)
        | Center        -> (0, 0)

    let isValidWord (word: string) (st: state) =
        Dictionary.lookup word st.dict

    let scoreWord (word: string) =
        word.Length
    
    let generateBestPossibleWord (st: state) (initAcc: string) (dict: Dict) (startCoord: coord) (direction: Direction) =
        let rec generateBestPossibleWordAux (acc : string) (hand : MultiSet<uint32>) (dict : Dict) (piecesOnBoard : Map<coord, (char * int)>) (coord : coord) (dir : Direction) =
            
            let start =
                match (piecesOnBoard.TryFind(coord)) with
                | Some (char, i)   ->  MultiSet.toList (MultiSet.addSingle (char) MultiSet.empty)
                | None             ->  MultiSet.toList (uint32MSToCharMS hand)
            
            let test = start

            let length = test.Length

            List.fold (fun (currentLongestWord : string) (char : char) ->
                
                let currentWord = (acc + char.ToString())
                
                match (step char dict) with
                | Some (isWord, children) ->
                    // Make sure we know which pieces have been used and which are left in our hand:
                    let piecesLeftInHand =
                        match (piecesOnBoard.TryFind coord) with
                        | Some _ -> hand
                        | None   -> (MultiSet.removeSingle (charToUint32 char) hand)
                    
                    let newCoordinate = (mergeCoords coord (dirToCoord direction) Add)
                    let nextWord = generateBestPossibleWordAux currentWord piecesLeftInHand children piecesOnBoard newCoordinate dir
                    
                    if isWord && currentWord.Length > currentLongestWord.Length && doesTileExist st newCoordinate then 
                        currentWord
                    else if nextWord.Length > currentLongestWord.Length 
                    then
                        nextWord
                    else
                        currentLongestWord
                    
                | None  -> currentLongestWord
                ) "" start
            
        generateBestPossibleWordAux initAcc st.hand dict st.piecesOnBoard startCoord direction

    let parseBotMove (st: state) ((word: string), (initCoord, dir): coord * Direction) : ((int * int) * (uint32 * (char * int))) list =
        let rec parseBotMoveAux (acc: ((int * int) * (uint32 * (char * int))) list) (chars: char list) (coord: coord) (direction: Direction) =

            match chars with
            | [] -> List.rev acc
            | x::xs -> 
                let charUint = charToUint32 x
                let charVal = charValues x
                let newCoord = mergeCoords coord (dirToCoord direction) Add
                let output = (coord, (charUint, (x, charVal)))

                match (isPieceOnTile st coord) with
                | true  ->
                    parseBotMoveAux acc xs (mergeCoords coord (dirToCoord direction) Add) direction
                | false ->
                    parseBotMoveAux (output::acc) xs newCoord direction
                    
        parseBotMoveAux [] [for char in word do char] initCoord dir