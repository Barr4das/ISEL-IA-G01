
:- module(prints, [
    print_list/1,
    print_map/1,
    print_coordinates/2
]).

print_list([]).
print_list([Head | Tail]) :-
    write(Head),
    print_list(Tail).
    
print_map([]).

print_map([Head | Tail]):-
    print_list(Head),
    write('\n'),
    print_map(Tail).

print_coordinates(X, Y) :-
    write("X: "),
    write(X),
    write("\nY: "),
    write(Y).