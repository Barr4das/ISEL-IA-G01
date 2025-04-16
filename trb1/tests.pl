test :-
    Board = [
        ['\u25CF', '.', '\u25CF', '.', '\u25CF', '.', '\u25CF', '.'],  
        ['.', '\u25CF', '.', '\u25CF', '.', '\u25CF', '.', '\u25CF'],  
        ['.', '.', '\u25CF', '.', '\u25CF', '.', '\u25CF', '.'],  
        ['.', '\u25CF', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '\u25CB', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB'], 
        ['\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB']   
    ],
    Board_size = 8,
    Color = white,
    player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),
    write(PlayerForcedMoves), nl.

test2 :-
    Board = [
        ['\u25CF', '.', '\u25CF', '.', '\u25CF', '.', '\u25CF', '.'],  
        ['.', '\u25CF', '.', '\u25CF', '.', '\u25CF', '.', '\u25CF'],  
        ['.', '.', '\u25CF', '.', '\u25CF', '.', '\u25CF', '.'],  
        ['.', '\u25CF', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '\u25CB', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB'], 
        ['\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB']   
    ],
    Board_size = 8,
    has_forced_move(Board, Board_size, 2, 4, PlayerForcedMoves),
    write(PlayerForcedMoves), nl.

test3 :-
    Board = [
        ['\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.'],  
        ['.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf'],  
        ['.', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.'],  
        ['.', '\u25cf', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '\u25CB', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB'], 
        ['\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB']   
    ],
    Board_size = 8,
    Color = white,
    player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),
    write(PlayerForcedMoves), nl,
    is_move_forced_valid(2,4,0,2, PlayerForcedMoves).

test4 :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '\u25cf', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '\u25cf', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '\u25CB', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    Board_size = 8,
    play(Board, Board_size, '\u25cb', Board_size, Board_size, 0, 0).