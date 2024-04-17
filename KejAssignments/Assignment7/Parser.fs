module ImpParser

    open Eval
    open Types

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
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

    let (.>*>.) p1 p2  = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    let punderscore = pchar '_'
    let pid =
        let initialChar = pletter <|> punderscore
        let rest = many (palphanumeric <|> punderscore)
        initialChar .>*>. rest |>> fun (a, b) -> System.String.Concat(a::b)

    
    let unop op a =
        op >*>. a
        
    let binop op a b = 
        a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let VarParse = pid |>> V <?> "Var"
    let NegParse = unop (pchar '-') AtomParse |>> (fun a -> Mul ((N -1), a)) <?> "Neg"
    

    let AexpParse = TermParse 

    let CParse, cref = createParserForwardedToRef<cExp>()

    let CharParse = between (pchar ''') (pchar ''') (palphanumeric <|> whitespaceChar) |>> C <?> "Char"
    let toUpperParse = unop pToUpper (parenthesise CParse) |>> ToUpper <?> "ToUpper"
    let toLowerParse = unop pToLower (parenthesise CParse) |>> ToLower <?> "ToLower"
    let intToCharParse = unop pIntToChar (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"
    let charValueParse : Parser<cExp> = unop pCharValue (parenthesise AexpParse) |>> CV <?> "CV"
    do cref := choice [charValueParse; intToCharParse;  toUpperParse; toLowerParse; CharParse]

    let charToIntParse = unop pCharToInt (parenthesise CParse) |>> CharToInt <?> "V"
    do aref := choice [charToIntParse; NegParse; PVParse; VarParse; NParse; ParParse; ]

    let CexpParse = CParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

