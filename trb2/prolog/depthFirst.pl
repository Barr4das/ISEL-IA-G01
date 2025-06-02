:- consult('sokoban.pl').

path(State, State, [State]).

path(InitialState, LastState, [LastState | Path]) :-
	path(InitialState, OneButLast, Path), 
	s(OneButLast, LastState, 1),          
	\+ member(LastState, Path). 

depth_first_iterative_deepening(InitialState, Solution) :-
	path(InitialState, GoalState, Solution),
	goal(GoalState).

test8 :-
    example_level_2(GameMap),
    map_loader(GameMap, State),
    %trace,
    depth_first_iterative_deepening(State, Solution),
    print_list(Solution),
    write("\n").