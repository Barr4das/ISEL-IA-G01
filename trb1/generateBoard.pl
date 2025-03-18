
print_item(Item) :- write(' '), write(Item), write(' ').

printList([]).

printList([Head | Tail]) :-
    print_item(Head),
    printList(Tail).

printBoard([]).

printBoard([Row| Tail]) :-
    write('   '),
    printList(Row),
    nl,
    printBoard(Tail),
    nl.

generateBoard(N, Board) :-
    length(Board, N), 
    length(Row, N),
    maplist(=('.'), Row),
    maplist(=(Row), Board).

%init(Board).