
/*
 * TEST FUNCTIONS
*/

:-module('tests', [
    test1/0,
    test2/0,
    test3/0,
    test4/0,
    test5/0,
    test6/0,
    test7/0,
    test8/0,
    test9/0,
    test10/0,
    test11/0,
    test12/0,
    test13/0,
    test14/0,
    test15/0,
    test16/0,
    test17/0
]).

test1 :-
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
    generate_empty_board(Board, Board_size),
    Player_rows = 3,
    fill_board(Board, Player_rows, 0, NewBoard),
    print_checkers(NewBoard, Board_size),
    bot(NewBoard, Board_size, white, 3, _).

test9 :-
    Board_size = 8,
    generate_empty_board(Board, Board_size),
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

test9 :-
    Board_size = 8,
    generate_empty_board(Board, Board_size),
    Player_rows = 3,
    fill_board(Board, Player_rows, 0, NewBoard),
    
    Player = white,

    player_legal_moves(NewBoard, Board_size, Player, LegalMoves, _),

    generate_boards_from_moves(NewBoard, LegalMoves, GeneratedBoards),


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

test10 :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '\u25cb', '.', '\u25cb', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '\u25cb', '.', '\u25cb', '.', '.'], 
        ['.', '.', '.', '.', '\u25cf', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    Board_size = 8,

    player_legal_moves(Board, Board_size, black, LegalMoves, _),

    adapt_moves(LegalMoves, ForcedStructured),

    generate_boards_from_moves(Board, Board_size, ForcedStructured, GeneratedBoards),

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

test11 :-
    Board_size = 8,
    generate_empty_board(Board, Board_size),
    fill_board(Board, 3, 0, Result),
    player_legal_moves(Result, Board_size, 'black', LegalMoves, _), 
    generate_boards_from_moves(Result, LegalMoves, GeneratedBoards),

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

test12 :-
    %Board = [
    %    [ '\u25cf', '.', '.', '.' ],
    %    [ '.' , '.', '.', '.'],
    %    [ '.' , '.', '.', '.'],
    %    [ '.','.' , '.', '\u25cb' ]
    %],
    Board_size = 4,
    generate_empty_board(InitialBoard, Board_size),
    Player_rows is (Board_size-2) // 2,
    fill_board(InitialBoard, Player_rows, 0, FilledBoard),
    play(FilledBoard, Board_size, '\u25cb', Board_size, Board_size, 1, 0).

test13 :-
Board_size = 4,
    generate_empty_board(Board, Board_size),
    fill_board(Board, 1, 0, Result),
    play(Result, Board_size, '\u25cb', Board_size, Board_size, 1, 0).

test14 :-
    Board = [
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '\u25cb', '.', '\u25cb', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '\u25cb', '.', '\u25cb', '.', '.', '.', '.'], 
            ['.', '.', '\u25cf', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.']   
        ],
        Board_size = 8,
        print_checkers(Board, Board_size),
        %minimax(BoardIn, Board_size, Player_color, BestSucc, Val) :-
        minimax(Board, Board_size, black, BoardBest, _),
        print_checkers(BoardBest, Board_size).

test15 :-
    Board = [
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '\u25cb', '.', '.', '.', '.', '.', '.'], 
            ['.', '.', '\u25cf', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.']   
        ],
    Board_size = 8,
    PlayerSymbol = '\u25cb',
    print_checkers(Board, Board_size), nl,
    capture(Board, 2, 6, 0, 4, AfterMoveBoard),
    play(AfterMoveBoard, Board_size, PlayerSymbol, Board_size, Board_size, 1, 0).

test16 :-
    Board = [
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '\u25cf', '.', '\u25cf', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.'],  
            ['.', '\u25cf', '.', '\u25cf', '.', '.', '.', '.'], 
            ['.', '.', '\u25cb', '.', '.', '.', '.', '.'],  
            ['.', '.', '.', '.', '.', '.', '.', '.']   
        ],
    Board_size = 8,
    PlayerColor = white,
    print_checkers(Board, Board_size), nl,
    Alpha = -9999,
    Beta = 9999,
    alphabeta( Board, Board_size, PlayerColor, PlayerColor, 3, Alpha, Beta, GoodBoard, Val),
    print_checkers(GoodBoard, Board_size).

test17 :-
    Board_size = 8,
    PlayerSymbol = '\u25cb',
    generate_empty_board(Board, Board_size),
    fill_board(Board, 3, 0, Result),
    play(Result, Board_size, PlayerSymbol, Board_size, Board_size, 1, 0).
