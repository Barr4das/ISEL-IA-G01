
letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K']).

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
    print_item(N),
    printList(Row),
    print_item(N),
    nl,
    N1 is N - 1,
    printBoard(Tail, N1).

print_checkers(Board) :-
    write('   '),
    letters(Letters),
    printList(Letters, 5),
    nl,
    printBoard(Board, 5),
    write('   '),
    printList(Letters, 5).

generateBoard(N, Board) :-
    length(Board, N), 
    length(Row, N),
    maplist(=('.'), Row),
    maplist(=(Row), Board).

%init(Board).

