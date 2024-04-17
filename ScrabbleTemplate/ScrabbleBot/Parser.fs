module internal Parser

    open Eval
    open ScrabbleUtil
    open StateMonad

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    // Have them set as "not yet implemented"
    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    type boardFun2 = coord -> Result<square option, Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' <?> "parenthesise"
    let curl p = pchar '{' >*>. p .>*> pchar '}' <?> "curl"

    let pid = pletter <|> pchar '_' .>>. many palphanumeric |>> fun (x,y) -> System.String (List.toArray (x :: y))

    
    let unop op a = op >*>. a
    let binop op a b =  a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CExpBaseParse, cref = createParserForwardedToRef<cExp>()

    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [SubParse; AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [ModParse; DivParse; MulParse; AtomParse]

    
    let NParse         = pint32 |>> N <?> "Int"
    let VParse         = pid |>> V <?> "Variable"
    let ParParse       = parenthesise TermParse
    let PVParse        = pPointValue >*>. ParParse |>> PV <?> "Point Value"
    let NegParse       = unop (pchar '-') AtomParse |>> (fun x -> Mul (N (-1), x)) <?> "Negation"
    let CharToIntParse = unop pCharToInt (parenthesise CExpBaseParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToIntParse; NegParse; PVParse; VParse ;NParse; ParParse]

    let AexpParse = TermParse 


    let ToUpperParse   = unop pToUpper (parenthesise CExpBaseParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse   = unop pToLower (parenthesise CExpBaseParse) |>> ToLower <?> "ToLower"
    let CParse         = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "Char"
    let CVParse        = unop pCharValue (parenthesise AexpParse) |>> CV <?> "Character Value"
    let IntToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"

    do cref := choice [CVParse; IntToCharParse; ToUpperParse; ToLowerParse; CParse]

    let CexpParse = CExpBaseParse

    let BexpParse = pstring "not implemented"

    let SFirst = createParserForwardedToRef<stm>()

    let stmntParse = SFirst


    let parseSquareProg (sqp: Map<int, string>) = 
        sqp 
        |> Map.map (fun _ p -> (stmntToSquareFun (getSuccess(run stmntParse p))))

    let parseBoardProg s (sqs: Map<int, square>) = 
        stmntToBoardFun (getSuccess(run stmntParse s)) sqs

    let mkBoard (bp : boardProg) : board =
        let squaresMap = bp.squares
        let squares = Map.map (fun _ squareProg -> parseSquareProg squareProg) squaresMap
        let defaultSquare = Map.find bp.usedSquare squaresMap
        { 
            center = bp.center
            defaultSquare = parseSquareProg defaultSquare
            squares = parseBoardProg bp.prog squares 
        }

    

