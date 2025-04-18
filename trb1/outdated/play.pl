play_old(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0) :-
    ( LastX =:= Board_size ->
        write("ENTREI YUPPIE"),nl,
        player_number(PlayerSymbol, PlayerNumber),
        (read_input(PlayerNumber, X1, Y1, X2, Y2) -> true ; play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)),

        (   \+ valid_coordinate(Board_size, X1, Y1);
            \+ valid_coordinate(Board_size, X2, Y2);
            \+ is_corresponding_player(PlayerNumber, Board, X1, Y1)
        ->
            write("Invalid input. Try again..."), nl,
            play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
        ;
            piece_color(PlayerSymbol, Color),
            player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),
            (   PlayerForcedMoves \= [] ->
                    (   is_move_forced_valid(X1, Y1, X2, Y2, PlayerForcedMoves) ->
                            capture(Board, X1, Y1, X2, Y2, AfterMoveBoard),
                            print_checkers(AfterMoveBoard, Board_size),
                            has_forced_move(AfterMoveBoard, Board_size, X2, Y2, ComboForcedMove),
                            (   ComboForcedMove \= [] ->
                                    play(AfterMoveBoard, Board_size, PlayerSymbol, X2, Y2, 0, 0)
                            ;   opponent_symbol(PlayerSymbol, Opp),
                                play(AfterMoveBoard, Board_size, Opp, Board_size, Board_size, 0, 0)
                            )
                    ;   write("Invalid move! Hint: You must capture a piece when able."), nl,
                        play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
                    )
                ;
                    nth0(Y1, Board, YList),
                    nth0(X1, YList, PosSymbol),
                    is_legal_move(Board, PosSymbol, X1, Y1, X2, Y2),
                    move(PosSymbol, X1, Y1, X2, Y2, Board, AfterMoveBoard),
                    opponent_symbol(PlayerSymbol, OpponentSymbol),
                    print_checkers(AfterMoveBoard, Board_size),
                    play(AfterMoveBoard, Board_size, OpponentSymbol, Board_size, Board_size, 0, 0)
            )
        )
    ;
        has_forced_move(Board, Board_size, LastX, LastY, [_, _, ForcedMoveList]),
        player_number(PlayerSymbol, PlayerNumber),
        (read_input(PlayerNumber, X1, Y1, X2, Y2) -> true ; play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)),

        (   \+ valid_coordinate(Board_size, X1, Y1);
            \+ valid_coordinate(Board_size, X2, Y2);
            (X1 =\= LastX; Y1 =\= LastY)
        ->
            write("Invalid input. Try again..."), nl,
            play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
        ;
            (   
                forced_moves_contains(X2, Y2, ForcedMoveList) ->
                    capture(Board, X1, Y1, X2, Y2, CapturedBoard),
                    print_checkers(CapturedBoard, Board_size),
                    has_forced_move(CapturedBoard, Board_size, X2, Y2, [_, _,ComboForcedMove]),
                    write("ComboForcedMove: "), write(ComboForcedMove),
                    (   
                        ComboForcedMove \= [] ->
                            write("I ENTERED HAS MORE MOVES CONDITION"), nl,
                            play(CapturedBoard, Board_size, PlayerSymbol, X2, Y2, 0, 0)
                        ;   
                        opponent_symbol(PlayerSymbol, Opp),
                        trace,
                        play(CapturedBoard, Board_size, Opp, Board_size, Board_size, 0, 0)
                    )
            ;   write("Invalid move! Hint: You must capture a piece when able."), nl,
                play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
            )
        )
    ).