namespace DysBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        numPlayers    : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        piecesOnBoard : Map<coord, (char * int)>
    }

    let mkState b d pn h num pt = {board = b; dict = d;  playerNumber = pn; hand = h; numPlayers = num; playerTurn = pt; piecesOnBoard = Map.empty}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let numPlayers st    = st.numPlayers
    let playerTurn st    = st.playerTurn
    let piecesOnBoard st = st.piecesOnBoard

    let nextTurn st numPlayers =
        match st.playerTurn with
        | n when n = numPlayers -> 1u
        | _ -> st.playerTurn + 1u

module Scrabble =
    open System.Threading
    open State

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            

            //sleep for 1 second
            Thread.Sleep(1000)

            debugPrint (sprintf "Player %d thinks there are %d players in the game\n" (State.playerNumber st) (State.numPlayers st))

            debugPrint (sprintf "Player %d thinks it's Player %d's turn\n" (State.playerNumber st) (State.playerTurn st))


            if (st.playerTurn = st.playerNumber) then
                if (isFreshBoard) then
                    let wordToPlay =
                        parseBotMove






            (*
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            if State.playerTurn st = State.playerNumber st
            then 
                //Print.printHand pieces (State.hand st)
                let input = System.Console.ReadLine();
                let move = RegEx.parseMove input;
                send cstream (SMPlay move)
                //send cstream SMPass
                debugPrint (sprintf "Player %d made a move\n" (State.playerNumber st))

            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            *)

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =  // This state needs to be updated
                    let NewPiecesMS = List.fold (fun acc (id, count) -> MultiSet.add id count acc) MultiSet.empty newPieces
                    let updatedHand =
                        ms
                        |> List.fold (fun acc (coord, (id, (char, point))) -> MultiSet.removeSingle id acc) st.hand // Remove old tiles
                        |> MultiSet.union NewPiecesMS                                                               // Add new pieces
                    {
                    st with 
                        playerTurn = nextTurn st st.numPlayers
                        hand = updatedHand
                    }
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' =
                    {
                    st with 
                        playerTurn = nextTurn st st.numPlayers
                    }

                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' =
                    {
                    st with 
                        playerTurn = nextTurn st st.numPlayers
                    }

                aux st'
            | RCM (CMPassed (pid)) ->
                (* Player passed. Update your state *)
                let st' =
                    {
                    st with 
                        playerTurn = nextTurn st st.numPlayers
                    }

                aux st'
            | RCM (CMGameOver _) ->
                // Game over. Do nothing and return
                let st' =
                    let gameOver = 0u
                    { 
                    st with playerTurn = gameOver 
                    }

                ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> 
                printfn "Gameplay Error:\n%A" err
                let st' =
                    let gameOver = 0u
                    { 
                    st with playerTurn = gameOver 
                    }
                
                aux st'

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn)
        