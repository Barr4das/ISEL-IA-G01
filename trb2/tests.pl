
test0() :-
    example_level_1(Level1),
    goal(Level1).

test1() :-
    ModifiedLevel = [
        ['#','#','#','#','#'],
        ['#', '@', '*', ' ', '#'],
        ['#','#','#','#','#']
    ],
    goal(ModifiedLevel).

test2 :-
    example_level_1(Level),
    find_player(Level, X, Y),
    print_coordinates(X, Y).

test3 :-
    example_level_2(Level),
    find_player(Level, X, Y),
    print_coordinates(X, Y).
