
letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L']).

is_queen('Q').
is_queen('q').

piece_color('○', white).
piece_color('●', black).
piece_color('q', white).
piece_color('Q', black).
piece_color('.', none).

opponent_color('○', black).
opponent_color('●', white).
opponent_color('q', black).
opponent_color('Q', white).
opponent_color('.', none).

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

% has_forced_move(Board, Board_size, PlayerPiece, XIn, YIn, ForcedMoves) :-

valid_coordinate(Board_size, X, Y) :-
    X >= 0, Y >= 0,
    X < Board_size, Y < Board_size.

is_capture_possible(Board, Board_size, XIn, YIn,  XTranslation, YTranslation, OpponentColor, XEnd, YEnd) :-
    X1 is XIn + XTranslation,
    Y1 is YIn + YTranslation,
    valid_coordinate(Board_size, X1, Y1),
    XEnd is XIn + (2 * XTranslation),
    YEnd is YIn + (2 * YTranslation),
    valid_coordinate(Board_size, XEnd, YEnd),
    nth0(Y1, Board, YList1),
    nth0(X1, YList1, XVal),
    piece_color(XVal, PieceColor1),
    PieceColor1 = OpponentColor,
    nth0(YEnd, Board, EndRow),
    nth0(XEnd, EndRow, EndPiece),
    EndPiece = '.'.

% FALTA TESTAR 
has_forced_move(Board, Board_size, XIn, YIn, ForcedMoves) :-
    nth0(YIn, Board, YList),
    nth0(XIn, YList, Piece),
    opponent_color(Piece, OpponentColor),
    findall([XIn, YIn, XEnd, YEnd], 
        (   
            is_capture_possible(Board, Board_size, XIn, YIn, -1, 1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn, 1, 1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn, -1, -1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn, 1, -1, OpponentColor, XEnd, YEnd)
        ), 
        ForcedMoves).

test :-
    Board_size = 8,
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

test_forced_moves :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '○', '.', '.', '.', '.'],  
        ['.', '.', '●', '.', '●', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    Board_size = 8,
    XIn = 3, YIn = 3,  % Coordenadas da peça branca '○'
    has_forced_move(Board, Board_size, XIn, YIn, ForcedMoves),
    write("2 Forced moves for piece at (3,3): "), write(ForcedMoves), nl,
    Board2 = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '●', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    has_forced_move(Board2, Board_size, XIn, YIn, ForcedMoves2),
    write("4 Forced moves for piece at (3,3): "), write(ForcedMoves2), nl,
    Board3 = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '○', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    has_forced_move(Board3, Board_size, XIn, YIn, ForcedMoves3),
    write("No Forced moves for piece at (3,3): "), write(ForcedMoves3), nl.


