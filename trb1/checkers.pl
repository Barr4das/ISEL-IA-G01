
letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L']).

is_queen('♕').
is_queen('♛').

piece_color('\u25CB', white).
piece_color('\u25CF', black).
piece_color('\u2655', white).
piece_color('\u265B', black).
piece_color('.', none).

opponent_color('\u25CB', black).
opponent_color('\u25CF', white).
opponent_color('\u2655', black).
opponent_color('\u265B', white).
opponent_color('.', none).

opponent_symbol('\u25cb', '\u25cf').
opponent_symbol('\u25cf', '\u25cb').

player_number('\u25cb', 1).
player_number('\u25cf', 2).
player_number('\u2655', 1).
player_number('\u265B', 2).

first([], []).
first(X, [X|_]).

print_item(Item) :- write(' '), write(Item), write(' ').

printList([]).

printList([Head | Tail]) :-
    print_item(Head),
    printList(Tail).

printList([], _).
printList(_, 0).
printList([Head | Tail], N) :-
    N > 0 -> 
        print_item(Head),
        N1 is N - 1,
        printList(Tail, N1).

printBoard([], _).
printBoard([], 0).

printBoard([Row| Tail], N) :-
    N > 0, N < 10 -> 
        write(' '),
        printBoardHelper([Row|Tail], N);
    printBoardHelper([Row|Tail], N).

printBoardHelper([Row|Tail], N) :-
    print_item(N),
    printList(Row),
    print_item(N),
    nl,
    N1 is N - 1,
    printBoard(Tail, N1).

print_checkers(Board, N) :-
    write('    '),
    letters(Letters),
    printList(Letters, N),
    nl,
    printBoard(Board, N),
    write('    '),
    printList(Letters, N),
    nl.

generateBoard(Board, N) :-
    length(Board, N), 
    length(Row, N),
    maplist(=('.'), Row),
    maplist(=(Row), Board).

add_pieces_even(PlayerSymbol, [_, H | T], [PlayerSymbol, H | T]).

add_pieces(_, [], _).
add_pieces(_, [_], [_]).

add_pieces(PlayerSymbol, [HI, HI2 | TI], [HO, HI2 | Rest]) :-
    add_pieces_even(PlayerSymbol, [HI, HI2 | TI], [HO, HI2 | _]),
    add_pieces(PlayerSymbol, TI, Rest).

fill_board([], _, _, []).

fill_board([HI | TI], Player_rows, Idx, [HO | TO]) :-
    Idx < Player_rows, !,
    New_idx is Idx + 1,
    process_row(HI, '\u25cf', Idx, HO),
    fill_board(TI, Player_rows, New_idx, TO).

fill_board([HI | TI], Player_rows, Idx, [HO | TO]) :-
    Idx >= Player_rows + 2, !,
    New_idx is Idx + 1,
    process_row(HI, '\u25cb', Idx, HO),
    fill_board(TI, Player_rows, New_idx, TO).

fill_board([HI | TI], Player_rows, Idx, [HI | TO]) :-
    New_idx is Idx + 1,
    fill_board(TI, Player_rows, New_idx, TO).

process_row(Row, Piece, Idx, Result) :-
    ( 1 =:= Idx mod 2 
    ->  reverse(Row, RevRow),
        add_pieces(Piece, RevRow, RevResult),
        reverse(RevResult, Result)
    ;   add_pieces(Piece, Row, Result)
    ).

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   nth0(Index,List,OldElem,Transfer),
   nth0(Index,NewList,NewElem,Transfer).

% only does the movement (no logic envolved)
move(PlayerSymbol, XStart, YStart, XFinish, YFinish, BoardIn, BoardOut) :-
    nth0(YStart, BoardIn, YList),
    replace_nth0(YList, XStart, PlayerSymbol, '.', NewYList),
    replace_nth0(BoardIn, YStart, YList, NewYList, BoardOutTemp),
    nth0(YFinish, BoardOutTemp, YList2), 
    replace_nth0(YList2, XFinish, '.', PlayerSymbol, NewYList2),
    replace_nth0(BoardOutTemp, YFinish, YList2, NewYList2, BoardOut).

string_length(String, Length) :-
    atom_chars(String, ListVar),
    length(ListVar, Length).

validate_input_n_args(First, Second) :-
    string_length(First, L1), string_length(Second, L2),
    L1 =:= 2, L2 =:= 2 -> true;
    write("INPUT ERROR"), nl, fail.

read_input(PlayerNumber, X1, Y1, X2, Y2) :-
    format("Player ~w (row/column): ", [PlayerNumber]),
    read_string(user, "\n", "\r", _, Response),
    split_string(Response, " ", "", [Start, Finish]), !,
    validate_input_n_args(Start, Finish),
    atom_chars(Start, [YIn1, XIn1]),
    atom_chars(Finish, [YIn2, XIn2]),
    atom_number(YIn1, Ycalc1),
    atom_number(YIn2, Ycalc2),
    Y1 is 8 - Ycalc1,
    Y2 is 8 - Ycalc2,
    letters(Letters),
    nth0(X1, Letters, XIn1),
    nth0(X2, Letters, XIn2).

valid_coordinate(Board_size, X, Y) :-
    X >= 0,
    Y >= 0,
    X < Board_size,
    Y < Board_size.

is_capture_possible(Board, Board_size, XIn, YIn,  XTranslation, YTranslation, OpponentColor, XEnd, YEnd) :-
    X1 is XIn + XTranslation,
    Y1 is YIn + YTranslation,
    valid_coordinate(Board_size, X1, Y1),
    XEnd is XIn + (2 * XTranslation),
    YEnd is YIn + (2 * YTranslation),
    valid_coordinate(Board_size, XEnd, YEnd),
    nth0(Y1, Board, YList1),
    nth0(X1, YList1, XVal),
    piece_color(XVal, PieceColor),
    PieceColor = OpponentColor,
    nth0(YEnd, Board, EndRow),
    nth0(XEnd, EndRow, EndPiece),
    EndPiece = '.'.

has_forced_move(Board, Board_size, XIn, YIn, ForcedMoves) :-
    nth0(YIn, Board, YList),
    nth0(XIn, YList, Piece),
    opponent_color(Piece, OpponentColor),
    findall([XEnd, YEnd], 
        (
            is_capture_possible(Board, Board_size, XIn, YIn, -1,  1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn,  1,  1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn, -1, -1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn,  1, -1, OpponentColor, XEnd, YEnd)
        ), 
        ForcedMovesList),
    (ForcedMovesList \= [] ->
        append([XIn, YIn], ForcedMovesList, ForcedMoves)
    ;
        ForcedMoves = []
    ).

player_forced_moves(Board, Board_size, Color, PlayerForcedMoves) :-
    MaxIdx is Board_size-1,
    findall(Move,
        (   
            between(0, MaxIdx, Y),
            between(0, MaxIdx, X),
            nth0(Y, Board, Row),
            nth0(X, Row, Piece),
            Piece \= '.',
            piece_color(Piece, Color),
            has_forced_move(Board, Board_size, X, Y, Move),
            Move \= []
        ),
        Moves),
    append(Moves, [], PlayerForcedMoves).

print_move(X1, Y1, X2, Y2) :-
    letters(L),
    nth0(Y1, L, Y1L),
    nth0(Y2, L, Y2L),
    write(X1), write(Y1L), write(" "), write(X2), write(Y2L), write(" ").
    
is_pos_empty(Board, X, Y) :-
    nth0(Y, Board, YList),
    nth0(X, YList, Pos),
    Pos == '.'.

%SEM RAINHAS
is_legal_move(Board, PieceType, XIn, YIn, TargetX, TargetY) :-
    PieceType == '\u25cb' ->
        is_pos_empty(Board, TargetX, TargetY),
        XT is abs(XIn - TargetX),
        YT is YIn - TargetY,
        YT =:= 1, 
        XT =:= 1;
    is_pos_empty(Board, TargetX, TargetY),
    XT is abs(XIn - TargetX),
    YT is YIn - TargetY,
    YT =:= -1, 
    XT =:= 1.

forced_moves_contains(_,_, []) :- fail.

forced_moves_contains(X,Y, [XL, YL | T]) :-
    X =:= XL, Y =:= YL ->
        true;
    forced_moves_contains(X,Y,T).


is_move_forced_valid(_, _, _, _, []) :- fail.

is_move_forced_valid(X1, Y1, X2, Y2, [[ForcedX, ForcedY, ForcedMovesList] | Tail]) :-
    (
        X1 =:= ForcedX, Y1 =:= ForcedY ->
            forced_moves_contains(X2, Y2, ForcedMovesList);
        is_move_forced_valid(X1, Y1, X2, Y2, Tail)
    ).
remove_piece(Board, X, Y, BoardOut) :-
    nth0(Y, Board, YList),
    nth0(X, YList, Item),
    replace_nth0(YList, X, Item, '.', NewYList),
    replace_nth0(Board, Y, YList, NewYList, BoardOut).

capture(Board,X1,Y1,X2,Y2,BoardOut) :-
    XDiff is (X2 - X1)/2 + X1,
    YDiff is (Y2 - Y1)/2 + Y1,
    remove_piece(Board, XDiff, YDiff, NewBoard),
    nth0(Y1, Board, YList),
    nth0(X1, YList, PosSymbol),
    move(PosSymbol, X1, Y1, X2, Y2, NewBoard, BoardOut).

is_corresponding_player(PlayerNumber, Board, X, Y) :-
    nth0(Y, Board, YList),
    nth0(X, YList, Item),
    player_number(Item, PlayerNumber).

checkers_move(Board, X1, Y1, X2, Y2, OutputBoard) :-
    nth0(Y1, Board, YList),
    nth0(X1, YList, PosSymbol),
    is_legal_move(Board, PosSymbol, X1, Y1, X2, Y2),
    move(PosSymbol, X1, Y1, X2, Y2, Board, OutputBoard).

piece_legal_moves(Board, X, Y, PieceLegalMoves) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece),
    Piece \= '.',
    piece_color(Piece, Color),
    (
        Color == white -> 
            Directions = [(-1, -1), (1, -1)]
        ; 
        Color == black -> 
            Directions = [(-1, 1), (1, 1)]
    ),
    findall([NewX, NewY],
        (
            member((DX, DY), Directions),
            NewX is X + DX,
            NewY is Y + DY,
            valid_coordinate(8, NewX, NewY),
            nth0(NewY, Board, NewRow),
            nth0(NewX, NewRow, TargetPiece),
            TargetPiece == '.'
        ),
        PieceLegalMoves
    ).

% BOARD, 8, black
player_legal_moves(Board, Board_size, PlayerColor, LegalMoves) :-
    player_forced_moves(Board, Board_size, PlayerColor, PlayerForcedMoves),
    PlayerForcedMoves \= [] ->
        (
            write("PlayerForcedMoves: "), write(PlayerForcedMoves), nl,
            LegalMoves = PlayerForcedMoves
        )
    ;
        (
            MaxIdx is Board_size - 1,
            findall(
                [X, Y, Moves],
                (
                    between(0, MaxIdx, X),
                    between(0, MaxIdx, Y),
                    nth0(Y, Board, Row),
                    nth0(X, Row, Piece),
                    piece_color(Piece, PlayerColor),
                    piece_legal_moves(Board, X, Y, Moves),
                    Moves \= []
                ),
                LegalMoves
            ),
            write("LegalMoves (no forced): "), write(LegalMoves), nl
        ).


play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0) :-

    % console prints
    print_checkers(Board, Board_size),
    player_number(PlayerSymbol, PlayerNumber),

    % read inputs
    (
        read_input(PlayerNumber, X1, Y1, X2, Y2) -> 
            true 
        ;
        write("Incorrect input format, try again."), nl,
        sleep(2),
        play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
    ),

    % input validation
    (   
        (
            \+ valid_coordinate(Board_size, X1, Y1);
            \+ valid_coordinate(Board_size, X2, Y2);
            \+ is_corresponding_player(PlayerNumber, Board, X1, Y1)
        ) ->
            write("Invalid input. Try again..."), nl,
            sleep(2),
            play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
        ;

        % REGULAR MOVE PLAY
        (
            LastX =:= 8 ->

            % verify forced moves
            piece_color(PlayerSymbol, Color),
            player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),

            (
                PlayerForcedMoves \= [] ->

                % check if given move is contained in forced moves list
                (
                    \+ is_move_forced_valid(X1, Y1, X2, Y2, PlayerForcedMoves) ->
                        write("Invalid move! Hint: You must capture a piece when able."), nl,
                        sleep(2),
                        play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
                ;

                    % perform capture
                    capture(Board, X1, Y1, X2, Y2, AfterMoveBoard),

                    % check if it's a COMBO move
                    has_forced_move(AfterMoveBoard, Board_size, X2, Y2, ComboForcedMove),
                    (
                        ComboForcedMove \= [] ->
                            play(AfterMoveBoard, Board_size, PlayerSymbol, X2, Y2, 0, 0)
                    ;
                        opponent_symbol(PlayerSymbol, Opp),
                        play(AfterMoveBoard, Board_size, Opp, Board_size, Board_size, 0, 0)
                    )
                )

                ;
                
                % no forced moves - proceed with regular move
                checkers_move(Board, X1, Y1, X2, Y2, MovedBoard),
                opponent_symbol(PlayerSymbol, Opp),
                play(MovedBoard, Board_size, Opp, Board_size, Board_size, 0, 0)
            )

            ;

            % COMBO MOVE

            % valid move verification
            (
                % error on this condition
                (
                    X1 =\= LastX; 
                    Y1 =\= LastY
                ) ->
                    write("Invalid input. Try again..."), nl,
                    sleep(2),
                    play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
                ;

                % check existing forced moves
                has_forced_move(Board, Board_size, LastX, LastY, ForcedMoveList),
                (
                    forced_moves_contains(X2, Y2, ForcedMoveList) ->
                        % perform capture
                        capture(Board, X1, Y1, X2, Y2, CapturedBoard),
                        % check if there's another forced move
                        has_forced_move(CapturedBoard, Board_size, X2, Y2, ComboForcedMoves),
                        (
                            ComboForcedMoves \= [] ->
                                play(CapturedBoard, Board_size, PlayerSymbol, X2, Y2, 0, 0)
                        ;
                            opponent_symbol(PlayerSymbol, Opp),
                            play(CapturedBoard, Board_size, Opp, Board_size, Board_size, 0, 0)
                        )
                ;
                    write("Invalid move! Hint: You must capture a piece when able."), nl,
                    sleep(2),
                    play(Board, Board_size, PlayerSymbol, LastX, LastY, 0, 0)
                )
            )
        )
    ).

checkers(Board_size) :-
    write("Welcome to the Checkers Prolog game!"), nl,

    % base board preparation
    generateBoard(InitialBoard, Board_size),
    Player_rows is (Board_size-2) // 2,
    fill_board(InitialBoard, Player_rows, 0, FilledBoard),

    % VERIFY IF PLAYING WITH BOT

    play(FilledBoard, Board_size, '\u25cb', Board_size, Board_size, 0, 0).

test8 :-
    Board = [
        ['\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.'],  
        ['.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf'],  
        ['\u25cf', '.', '.', '.', '\u25cf', '.', '.', '.'],  
        ['.', '\u25cf', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '\u25CB', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '\u25cf', '.', '.', '.', '\u25CB'], 
        ['\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB']   
    ],
    Board_size = 8,
    print_checkers(Board, Board_size),
    player_forced_moves(Board, Board_size, white, PlayerForcedMoves),
    write("PlayerForcedMoves: "), write(PlayerForcedMoves), nl.

test9 :-
    Board_size = 8,
    generateBoard(Board, Board_size),
    Player_rows = 3,
    fill_board(Board, Player_rows, 0, NewBoard),
    print_checkers(NewBoard, Board_size),
    player_legal_moves(NewBoard, Board_size, white, PlayerForcedMoves).
