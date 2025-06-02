
/*
    BASE GAME TILES AND HELP FUNCTIONS
*/

:- module(definitions, [
    wall/1,
    player/1,
    goal/1,
    score/1,
    floor/1,
    is_not_over/1,
    box/1,  
    player_at/2,
    box_at/2,
    dir/2
]).

wall('#').
player('@').
player('+').
goal('+').
goal('.').
goal('*').
score('*').
floor(' ').
box('$').
box('*').

is_not_over('.').
is_not_over('+').

player_at(_X, _Y).
box_at(_X, _Y).

dir(up, (0,-1)).
dir(down, (0,1)).
dir(left, (-1,0)).
dir(right, (1,0)).