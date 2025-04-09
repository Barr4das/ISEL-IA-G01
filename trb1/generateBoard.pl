
letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L']).

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
    print_item(Head),
    N1 is N - 1,
    printList(Tail, N1).

printBoard([], _).
printBoard([], 0).

printBoard([Row| Tail], N) :-
    N < 10 -> 
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
    process_row(HI, '\u25cb', Idx, HO),
    fill_board(TI, Player_rows, New_idx, TO).

fill_board([HI | TI], Player_rows, Idx, [HO | TO]) :-
    Idx >= Player_rows + 2, !,
    New_idx is Idx + 1,
    process_row(HI, '\u25cf', Idx, HO),
    fill_board(TI, Player_rows, New_idx, TO).

fill_board([HI | TI], Player_rows, Idx, [HI | TO]) :-
    New_idx is Idx + 1,
    fill_board(TI, Player_rows, New_idx, TO).

process_row(Row, Piece, Idx, Result) :-
    (   1 =:= Idx mod 2 
    ->  reverse(Row, RevRow),
        add_pieces(Piece, RevRow, RevResult),
        reverse(RevResult, Result)
    ;   add_pieces(Piece, Row, Result)
    ).

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   nth0(Index,List,OldElem,Transfer),
   nth0(Index,NewList,NewElem,Transfer).

% only does the movement (no logic envolved)
play(PlayerSymbol, XStart, YStart, XFinish, YFinish, BoardIn, BoardOut) :-
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

up_right(Board, Board_size, XIn, YIn, XOut, YOut, PieceSymbol) :-
    YOut is YIn - 1,
    XOut is XIn + 1,
    BoardSizeDec is Board_size - 1,
    YOut >= 0, XOut =< BoardSizeDec -> 
        nth0(YOut, Board, YList),
        nth0(XOut, YList, PieceSymbol);
    write("NO UP RIGHT POS"), nl, fail.

up_left(Board, XIn, YIn, XOut, YOut, PieceSymbol) :-
    YOut is YIn - 1,
    XOut is XIn - 1,
    YOut >= 0, XOut >= 0 -> 
        nth0(YOut, Board, YList),
        nth0(XOut, YList, PieceSymbol);
    write("NO UP LEFT POS"), nl, fail.

down_right(Board, Board_size, XIn, YIn, XOut, YOut, PieceSymbol) :-
    YOut is YIn + 1,
    XOut is XIn + 1,
    YOut =< Board_size, XOut =< Board_size ->
        nth0(YOut, Board, YList),
        nth0(XOut, YList, PieceSymbol);
    write("NO DOWN RIGHT POS"), nl, fail.

down_left(Board, Board_size, XIn, YIn, XOut, YOut, PieceSymbol) :-
    YOut is YIn + 1,
    XOut is XIn - 1,
    YOut =< Board_size, XOut >= 0 ->
        nth0(YOut, Board, YList),
        nth0(XOut, YList, PieceSymbol);
    write("NO DOWN RIGHT POS"), nl, fail.

opposite_piece(Player, Color) :-
    Player =:= '●'; Player =:= 'Q' ->
        Color = "WHITE";
    Color = "BLACK".

% Board, 8, ○, 7, 7, [] 
has_forced_move(Board, Board_size, PlayerPiece, XIn, YIn, ForcedMoves) :-
    PlayerPiece =:= 'Q'; PlayerPiece =:= 'q' ->
        %todo
        write("NOT YET IMPLEMENTED");
    down_left(Board, Board_size, XIn, YIn, XOut, YOut, PieceDLeft),
    % todo
    opposite_piece(PlayerPiece, OpponentColor),
    opposite_piece(PieceDLeft, PlayerColor),
    
    PlayerColor \= Opponent ->
        down_left(Board, Board_size, XOut, YOut, XOut2, YOut2, PieceDLeft2),
        PieceDLeft2 =:= '.' ->
            append(ForcedMoves, [XIn, YIn, XOut2, YOut2]);
        %nothing

    down_right(Board, Board_size, XIn, YIn, XOut, YOut, PieceDRight),
    % todo
    opposite_piece(PlayerPiece, OpponentColor),
    opposite_piece(PieceDRight, PlayerColor),

    PlayerColor \= Opponent ->
        down_right(Board, Board_size, XOut, YOut, XOut2, YOut2, PieceDRight2),
        PieceDRight2 =:= '.' ->
            append(ForcedMoves, [XIn, YIn, XOut2, YOut2]);
        %nothing

    up_right(Board, Board_size, XIn, YIn, XOut, YOut, PieceURight),
    % todo
    opposite_piece(PlayerPiece, OpponentColor),
    opposite_piece(PieceURight, PlayerColor),

    PlayerColor \= Opponent ->
        up_right(Board, Board_size, XOut, YOut, XOut2, YOut2, PieceURight2),
        PieceURight2 =:= '.' ->
            append(ForcedMoves, [XIn, YIn, XOut2, YOut2]);
        %nothing

    up_left(Board, XOut, YOut, XOut2, YOut2, PieceULeft2),
    % todo
    opposite_piece(PlayerPiece, OpponentColor),
    opposite_piece(PieceULeft, PlayerColor),

    PlayerColor \= Opponent ->
        up_left(Board, Board_size, XOut, YOut, XOut2, YOut2, PieceULeft2),
        PieceULeft2 =:= '.' ->
            append(ForcedMoves, [XIn, YIn, XOut2, YOut2]);
        %nothing


test :-
    Board_size = 4,
    Player_rows is (Board_size-2) // 2,
    generateBoard(Board, Board_size),
    fill_board(Board, Player_rows, 0, FinalBoard),
    print_checkers(FinalBoard, Board_size),
    read_input(1, X1, Y1, X2, Y2).
    %down_right(FinalBoard, Board_size, X1, Y1, XOut, YOut, PieceSymbol),
    %print_item(XOut), print_item(YOut), print_item(PieceSymbol), nl.
    %print_item(Y1), print_item(X1), nl,
    %print_item(Y2), print_item(X2), nl,
    %play('\u25cb', X1, Y1, X2, Y2, FinalBoard, FinalFinalBoard),
    %print_checkers(FinalFinalBoard, Board_size).
