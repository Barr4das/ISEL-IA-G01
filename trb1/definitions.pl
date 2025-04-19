
/*
 * BASE SYMBOL AND COLOR LOGIC DEFINITIONS FOR CHECKERS
*/

:- module(definitions, [
    letters/1,
    is_queen/1,
    piece_color/2,
    opponent_color/2,
    opponent_symbol/2,
    player_number/2
]).


letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L']).

is_queen('♕').
is_queen('♛').

piece_color('\u25cb', white).
piece_color('\u25cf', black).
piece_color('\u2655', white).
piece_color('\u265B', black).
piece_color('.', none).

opponent_color('\u25cb', black).
opponent_color('\u25cf', white).
opponent_color('\u2655', black).
opponent_color('\u265B', white).
opponent_color('.', none).
opponent_color(white, black).
opponent_color(black, white).

opponent_symbol('\u25cb', '\u25cf').
opponent_symbol('\u25cf', '\u25cb').

player_number('\u25cb', 1).
player_number('\u25cf', 2).
player_number('\u2655', 1).
player_number('\u265B', 2).