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
        ['.', '.', '\u25cb', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    Board_size = 8,
    play(Board, Board_size, '\u25cb', Board_size, Board_size, 0, 0).


test5 :-
    Board = [
        ['\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.'],  
        ['.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf'],  
        ['\u25cf', '.', '.', '.', '\u25cf', '.', '.', '.'],  
        ['.', '\u25cf', '.', '.', '.', '\u25cf', '.', '.'],  
        ['.', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '\u25CB'], 
        ['\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB']   
    ],
    Board_size = 8,
    play(Board, Board_size, '\u25cf', Board_size, Board_size, 0, 0).
    %has_forced_move(Board, Board_size, 5, 3, ComboForcedMove),
    %write("ComboForcedMove: "), write(ComboForcedMove), nl.

test6 :-
    Board = [
        ['\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.'],  
        ['.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf'],  
        ['\u25cf', '.', '.', '.', '\u25cf', '.', '.', '.'],  
        ['.', '\u25cf', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '\u25CB', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '\u25cf', '.', '.', '.', '\u25CB'], 
        ['\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB']   
    ],
    Board_size = 8,
    %play(Board, Board_size, '\u25cf', Board_size, Board_size, 0, 0).
    has_forced_move(Board, Board_size, 3, 5, ComboForcedMove),
    write("ComboForcedMove: "), write(ComboForcedMove), nl.

test7 :-
    Board = [
        ['\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.'],  
        ['.', '\u25cf', '.', '\u25cf', '.', '\u25cf', '.', '\u25cf'],  
        ['\u25cf', '.', '.', '.', '\u25cf', '.', '.', '.'],  
        ['.', '\u25cf', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '\u25CB', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '\u25cf', '.', '.', '.', '\u25CB'], 
        ['\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.'],  
        ['.', '\u25CB', '.', '\u25CB', '.', '\u25CB', '.', '\u25CB']   
    ],
    Board_size = 8,
    print_checkers(Board, Board_size),
    player_forced_moves(Board, Board_size, white, PlayerForcedMoves),
    write("PlayerForcedMoves: "), write(PlayerForcedMoves), nl.

test8 :-
    Board_size = 8,
    generateBoard(Board, Board_size),
    Player_rows = 3,
    fill_board(Board, Player_rows, 0, NewBoard),
    print_checkers(NewBoard, Board_size),
    bot(NewBoard, Board_size, white, 3, BestMove).

test9 :-
    Board_size = 8,
    generateBoard(Board, Board_size),
    Player_rows = 3,
    fill_board(Board, Player_rows, 0, NewBoard),
    
    Player = white,

    player_legal_moves(NewBoard, Board_size, Player, LegalMoves, Forced),

    generate_boards_from_moves(NewBoard, LegalMoves, Forced, GeneratedBoards),


    forall(
        (
            member(BoardList, GeneratedBoards),
            member(BoardItem, BoardList)
        ),
        (
            print_checkers(BoardItem, Board_size),
            nl
        )
    ).