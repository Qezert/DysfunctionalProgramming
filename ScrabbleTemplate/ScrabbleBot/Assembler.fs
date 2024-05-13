module internal Assembler

    let isFreshBoard (st) =
        (st.piecesOnBoard |> Map.isEmpty)

    let parseBotMove (st) =