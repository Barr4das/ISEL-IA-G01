
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
    printList(Letters, N).

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

test:-
    Board_size = 8,
    generateBoard(Board, Board_size),
    Player_rows is (Board_size-2) // 2,
    fill_board(Board, Player_rows, 0, FinalBoard),
    print_checkers(FinalBoard, Board_size).

test2:-
    Board_size = 10,
    generateBoard(Board, Board_size),
    Player_rows is (Board_size-2) // 2,
    fill_board(Board, Player_rows, 0, FinalBoard),
    print_checkers(FinalBoard, Board_size).

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   nth0(Index,List,OldElem,Transfer),
   nth0(Index,NewList,NewElem,Transfer).

%play(YStart, XStart, YFinish, XFinish, BoardIn, BoardOut) :-
%    nht1(YStart, BoardIn, XList),
%    nht1(XIndex, XList, XStart),
%    replace().

test_replace :-
    List = [0,1,2,3,4,5,6,7,8,9],
    replace_nth0(List, 3, 4, 'djuro', Output),
    write(Output).