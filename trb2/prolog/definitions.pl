
/*
    BASE GAME TILES AND HELP FUNCTIONS
*/

:- module(definitions, [
    wall_symbol/1,
    floor_symbol/1,
    gate_symbol/1,
    player_symbol/1,
    box_symbol/1,
    goal_symbol/1,  
    final_goal_symbol/1,
    player_at/2,
    box_at/2,
    dynamic gate_at/2,
    dir/2
]).

wall_symbol('#').
floor_symbol(' ').
gate_symbol('%').
player_symbol('@').
player_symbol('+').
box_symbol('$').
box_symbol('*').
goal_symbol('+').
goal_symbol('.').
goal_symbol('*').
final_goal_symbol('?').

player_at(_X, _Y).
box_at(_X, _Y).
:- dynamic gate_at/2.
gate_at(_X, _Y).
gate_at(-1, -1).

dir(up, (0,-1)).
dir(down, (0,1)).
dir(left, (-1,0)).
dir(right, (1,0)).