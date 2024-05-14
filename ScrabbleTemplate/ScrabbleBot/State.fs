module internal State

    open ScrabbleUtil
    open Parser

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        numPlayers    : uint32
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        piecesOnBoard : Map<coord, (char * int)>
    }
