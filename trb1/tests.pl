test :-
    Board_size = 8,
    Player_rows is (Board_size-2) // 2,
    generateBoard(Board, Board_size),
    fill_board(Board, Player_rows, 0, FinalBoard),
    print_checkers(FinalBoard, Board_size),
    read_input(1, X1, Y1, X2, Y2),
    %down_right(FinalBoard, Board_size, X1, Y1, XOut, YOut, PieceSymbol),
    %print_item(XOut), print_item(YOut), print_item(PieceSymbol), nl.
    print_item(Y1), print_item(X1), nl,
    print_item(Y2), print_item(X2), nl,
    move('\u25cf', X1, Y1, X2, Y2, FinalBoard, FinalFinalBoard),
    print_checkers(FinalFinalBoard, Board_size).

test_forced_moves :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '○', '.', '.', '.', '.'],  
        ['.', '.', '●', '.', '●', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    Board_size = 8,
    XIn = 3, YIn = 3,  % Coordenadas da peça branca '○'
    has_forced_move(Board, Board_size, XIn, YIn, ForcedMoves),
    %[ X, Y | FMoves]
    write("should return : [3,3,[1,5],[5,5]] returned : "), write(ForcedMoves), nl,
    Board2 = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '●', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    has_forced_move(Board2, Board_size, XIn, YIn, ForcedMoves2),
    write("should return : [3,3,[1,5],[5,5],[1,1],[5,1]] returned : "), write(ForcedMoves2), nl,
    Board3 = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '○', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    has_forced_move(Board3, Board_size, XIn, YIn, ForcedMoves3),
    write("should return : [3,3] returned : "), write(ForcedMoves3), nl,
    Board4 = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '○', '.', '.', '.', '.'],  
        ['.', '.', '●', '.', '●', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    Board_size = 8,
    XIn1 = 2, YIn1 = 4,  % Coordenadas da peça preta '●'
    has_forced_move(Board4, Board_size, XIn1, YIn1, ForcedMoves4),
    %[ X, Y | FMoves]
    write("should return : [2,4,[4,2]] returned : "), write(ForcedMoves4), nl,
    XIn2 = 4, YIn2 = 4,  % Coordenadas da peça preta '●'
    has_forced_move(Board4, Board_size, XIn2, YIn2, ForcedMoves5),
    %[ X, Y | FMoves]
    write("should return : [4,4,[2,2]] returned : "), write(ForcedMoves5), nl.

test_player_forced_moves :-
    Board = [
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '●', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '○', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.'], 
        ['.', '.', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '.', '.', '.', '.', '.']   
    ],
    Board_size = 8,
    Color = white,
    player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),
    write("should return : [[3,3,[1,5],[5,5]]] or [[2,4,[4,2]],[4,4,[2,2]]] returned : "), write(PlayerForcedMoves), nl.

test_player_forced_moves_case1 :-
    Board = [
        ['●', '.', '●', '.', '●', '.', '●', '.'],  
        ['.', '●', '.', '●', '.', '●', '.', '●'],  
        ['.', '.', '●', '.', '●', '.', '●', '.'],  
        ['.', '●', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '○', '.', '○', '.', '○'], 
        ['○', '.', '○', '.', '○', '.', '○', '.'],  
        ['.', '○', '.', '○', '.', '○', '.', '○']   
    ],
    Board_size = 8,
    Color = white,
    player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),
    write(PlayerForcedMoves), nl.

test_player_forced_moves_case2 :-
    Board = [
        ['●', '.', '●', '.', '●', '.', '●', '.'],  
        ['.', '●', '.', '●', '.', '●', '.', '●'],  
        ['.', '.', '●', '.', '●', '.', '●', '.'],  
        ['.', '●', '.', '.', '.', '.', '.', '.'],  
        ['.', '.', '○', '.', '.', '.', '.', '.'],  
        ['.', '.', '.', '○', '.', '○', '.', '○'], 
        ['○', '.', '○', '.', '○', '.', '○', '.'],  
        ['.', '○', '.', '○', '.', '○', '.', '○']   
    ],
    Board_size = 8,
    Color = black,
    player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),
    write(PlayerForcedMoves), nl.