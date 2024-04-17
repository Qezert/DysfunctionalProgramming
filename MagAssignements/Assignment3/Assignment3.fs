module Assignment3

    open Types
        
    let rec arithEvalSimple (a:aExp) =
       match a with
       | N x -> x
       | Add (x, y) -> arithEvalSimple x + arithEvalSimple y
       | Sub (x, y) -> arithEvalSimple x - arithEvalSimple y
       | Mul (x, y) -> arithEvalSimple x * arithEvalSimple y
    
    let rec arithEvalState (a:aExp) (s:Map<string, int>) =
       match a with
       | N x -> x
       | V x -> if s.ContainsKey(x) then s.Item(x) else 0
       | Add (x, y) -> arithEvalState x s + arithEvalState y s
       | Sub (x, y) -> arithEvalState x s - arithEvalState y s
       | Mul (x, y) -> arithEvalState x s * arithEvalState y s
              
              
    let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]

    let rec arithEval (a: aExp) (w: word) (s: Map<string, int>) =
       match a with
       | N x -> x
       | WL -> w.Length
       | PV x -> snd w.[arithEval x w s]
       | V x -> if s.ContainsKey(x) then s.Item(x) else 0
       | Add (x, y) -> arithEval x w s + arithEval y w s
       | Sub (x, y) -> arithEval x w s - arithEval y w s
       | Mul (x, y) -> arithEval x w s * arithEval y w s


    type cExp =
       | C  of char      (* Character value *)
       | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
       | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
       | CV of aExp      (* Character lookup at word index *)

    let rec charEval (c: cExp) (w: word) (s: Map<string, int>) =
       match c with
       | C x -> x
       | ToUpper x -> System.Char.ToUpper(charEval x w s)
       | ToLower x -> System.Char.ToLower(charEval x w s)
       | CV x -> fst w.[arithEval x w s]

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp           (* boolean not *)
       | Conj of bExp * bExp   (* boolean conjunction *)

       | IsDigit of cExp       (* check for digit *)
       | IsLetter of cExp      (* check for letter *)
       | IsVowel of cExp       (* check for vowel *)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)
    
    let isVowel (c: char) =
       match System.Char.ToLower(c) with
       | 'a' | 'e' | 'i' | 'u' | 'o' | 'y' | 'æ' | 'ø' | 'å' -> true
       | _ -> false

    let rec boolEval (b: bExp) (w: word) (s: Map<string, int>) =
       match b with
       | TT -> true
       | FF -> false
       | AEq (x1, x2) -> (arithEval x1 w s) = (arithEval x2 w s)
       | ALt (x1, x2) -> (arithEval x1 w s) < (arithEval x2 w s)
       | Not x -> not (boolEval x w s)
       | Conj (x1, x2) -> (boolEval x1 w s) && (boolEval x2 w s)
       | IsDigit x -> System.Char.IsDigit(charEval x w s)
       | IsLetter x -> System.Char.IsLetter(charEval x w s)
       | IsVowel x -> isVowel (charEval x w s)
        
    let isConsonant _ = failwith "not implemented"

    type stmnt =
       | Skip                        (* does nothing *)
       | Ass of string * aExp        (* variable assignment *)
       | Seq of stmnt * stmnt        (* sequential composition *)
       | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
       | While of bExp * stmnt       (* while statement *)

    let evalStmnt _ = failwith "not implemented"

    let stmntToSquareFun (_ : stmnt) : squareFun = fun _ _ _ -> 0
    
    let singleLetterScore : squareFun = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
    let doubleLetterScore : squareFun = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
    let tripleLetterScore : squareFun = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))

    let doubleWordScore : squareFun = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
    let tripleWordScore : squareFun = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

    
    let oddConsonants = Skip // Replace this with your version of oddConsonants

    type square2 = (int * stmnt) list
    
    let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
    let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
    let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]

    let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
    let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

    let calculatePoints2 _ = failwith "not implemented"