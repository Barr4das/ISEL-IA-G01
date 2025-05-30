
/*
    BASE GAME TILES AND HELP FUNCTIONS
*/

:- module(definitions, [
    wall/1,
    player/1,
    objective/1,
    score/1,
    floor/1,
    is_not_over/1,
    box/1,  
    player_at/2,
    box_at/2
]).

wall('#').
player('@').
player('+').
objective('+').
objective('.').
objective('*').
score('*').
floor(' ').
box('$').
box('*').

is_not_over('.').
is_not_over('+').

player_at(_X, _Y).
box_at(_X, _Y).