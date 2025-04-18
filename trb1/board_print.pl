:-module('board_print', [
    print_item/1,
    print_list/1,
    print_list/2,
    print_rows/2,
    print_checkers/2
]).

/*
 * print_item(Item:Any) 
 * Auxiliary print function w/ spacing
 * Example: 
    print_item(1).
     1 
    true .
*/
print_item(Item) :- write(' '), write(Item), write(' ').

/*
 * print_list([Head|Tail]:List<Any>) 
 * Auxiliary print functions for lists
 * Example:
    print_list(["L", "I", "S", "T"]).
     L  I  S  T
    true .
*/
print_list([]).

print_list([Head | Tail]) :-
    print_item(Head),
    print_list(Tail).

/*
 * print_list([Head|Tail]:List<Any>,N:UInt) 
 * Auxiliary print function for spaced list items which prints first N items of given List
 * Example:
    print_list(["L", "I", "S", "T"], 2).
     L  I  
    true .
*/
print_list([], _).
print_list(_, 0).
print_list([Head | Tail], N) :-
    N > 0 -> 
        print_item(Head),
        N1 is N - 1,
        print_list(Tail, N1).

/*
 * print_rows(Board:List<List<Any>>,N:UInt)
 * Print function for game board w/ height identifier on the left and right side
 * Example:
    print_rows([ ['.', '.'], ['.', '.'] ], 2).
     2  .  .  2 
     1  .  .  1
    true .
*/
print_rows([], _).

print_rows([], 0).

print_rows([Row| Tail], N) :-
    (
        N < 10 -> 
            write(' ');
        true
    ),
    print_item(N),
    print_list(Row),
    print_item(N),
    nl,
    N1 is N - 1,
    print_rows(Tail, N1).

/*
 * print_checkers(Board:List<List<Any>>,N:UInt)
 * Prints the board with identified rows and columns where N is the board size
 * Example:
        A  B 
     2  .  .  2
     1  .  .  1
        A  B
    true .
*/
print_checkers(Board, N) :-
    write('    '),
    letters(Letters),
    print_list(Letters, N),
    nl,
    print_rows(Board, N),
    (
        N < 10 ->
            write('    ');
        write('    ')
    ),
    print_list(Letters, N),
    nl.