:- use_module('definitions').
:- use_module('tests').
:- use_module('board_print').
:- use_module('board_generate').

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
   nth0(Index,List,OldElem,Transfer),
   nth0(Index,NewList,NewElem,Transfer).

move(PlayerSymbol, XStart, YStart, XFinish, YFinish, BoardIn, BoardOut) :-
    nth0(YStart, BoardIn, YList),
    replace_nth0(YList, XStart, PlayerSymbol, '.', NewYList),
    replace_nth0(BoardIn, YStart, YList, NewYList, BoardOutTemp),
    nth0(YFinish, BoardOutTemp, YList2), 
    replace_nth0(YList2, XFinish, '.', PlayerSymbol, NewYList2),
    replace_nth0(BoardOutTemp, YFinish, YList2, NewYList2, BoardOut).

string_length(String, Length) :-
    atom_chars(String, ListVar),
    length(ListVar, Length).

validate_input_n_args(First, Second) :-
    string_length(First, L1), string_length(Second, L2),
    L1 =:= 2, L2 =:= 2 -> true;
    write("INPUT ERROR"), nl, fail.

read_input(Board_size, PlayerNumber, X1, Y1, X2, Y2) :-
    format("Player ~w (row/column): ", [PlayerNumber]),
    read_string(user, "\n", "\r", _, Response),
    split_string(Response, " ", "", [Start, Finish]), !,
    validate_input_n_args(Start, Finish),
    atom_chars(Start, [YIn1, XIn1]),
    atom_chars(Finish, [YIn2, XIn2]),
    upcase_atom(XIn1, XUp1),
    upcase_atom(XIn2, XUp2),
    atom_number(YIn1, Ycalc1),
    atom_number(YIn2, Ycalc2),
    Y1 is Board_size - Ycalc1,
    Y2 is Board_size - Ycalc2,
    letters(Letters),
    nth0(X1, Letters, XUp1),
    nth0(X2, Letters, XUp2).

valid_coordinate(Board_size, X, Y) :-
    X >= 0,
    Y >= 0,
    X < Board_size,
    Y < Board_size.

is_capture_possible(Board, Board_size, XIn, YIn,  XTranslation, YTranslation, OpponentColor, XEnd, YEnd) :-
    X1 is XIn + XTranslation,
    Y1 is YIn + YTranslation,
    valid_coordinate(Board_size, X1, Y1),
    XEnd is XIn + (2 * XTranslation),
    YEnd is YIn + (2 * YTranslation),
    valid_coordinate(Board_size, XEnd, YEnd),
    nth0(Y1, Board, YList1),
    nth0(X1, YList1, XVal),
    piece_color(XVal, PieceColor),
    PieceColor = OpponentColor,
    nth0(YEnd, Board, EndRow),
    nth0(XEnd, EndRow, EndPiece),
    EndPiece = '.'.

has_forced_move(Board, Board_size, XIn, YIn, ForcedMoves) :-
    nth0(YIn, Board, YList),
    nth0(XIn, YList, Piece),
    opponent_color(Piece, OpponentColor),
    findall([XEnd, YEnd], 
        (
            is_capture_possible(Board, Board_size, XIn, YIn, -1,  1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn,  1,  1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn, -1, -1, OpponentColor, XEnd, YEnd);
            is_capture_possible(Board, Board_size, XIn, YIn,  1, -1, OpponentColor, XEnd, YEnd)
        ), 
        ForcedMovesList),
    (ForcedMovesList \= [] ->
        append([XIn, YIn], ForcedMovesList, ForcedMoves)
    ;
        ForcedMoves = []
    ).

player_forced_moves(Board, Board_size, Color, PlayerForcedMoves) :-
    MaxIdx is Board_size-1,
    findall(Move,
        (   
            between(0, MaxIdx, Y),
            between(0, MaxIdx, X),
            nth0(Y, Board, Row),
            nth0(X, Row, Piece),
            Piece \= '.',
            piece_color(Piece, Color),
            has_forced_move(Board, Board_size, X, Y, Move),
            Move \= []
        ),
        Moves),
    append(Moves, [], PlayerForcedMoves).

print_move(X1, Y1, X2, Y2) :-
    letters(L),
    nth0(Y1, L, Y1L),
    nth0(Y2, L, Y2L),
    write(X1), write(Y1L), write(" "), write(X2), write(Y2L), write(" ").
    
is_pos_empty(Board, X, Y) :-
    nth0(Y, Board, YList),
    nth0(X, YList, Pos),
    Pos == '.'.

is_legal_move(Board, PieceType, XIn, YIn, TargetX, TargetY) :-
    PieceType == '\u25cb' ->
        is_pos_empty(Board, TargetX, TargetY),
        XT is abs(XIn - TargetX),
        YT is YIn - TargetY,
        YT =:= 1, 
        XT =:= 1;
    is_pos_empty(Board, TargetX, TargetY),
    XT is abs(XIn - TargetX),
    YT is YIn - TargetY,
    YT =:= -1, 
    XT =:= 1.

forced_moves_contains(_,_, []) :- fail.

forced_moves_contains(X,Y, [XL, YL | T]) :-
    X =:= XL, Y =:= YL ->
        true;
    forced_moves_contains(X,Y,T).

is_move_forced_valid(_, _, _, _, []) :- fail.

is_move_forced_valid(X1, Y1, X2, Y2, [[ForcedX, ForcedY, ForcedMovesList] | Tail]) :-
    (
        X1 =:= ForcedX, Y1 =:= ForcedY ->
            forced_moves_contains(X2, Y2, ForcedMovesList);
        is_move_forced_valid(X1, Y1, X2, Y2, Tail)
    ).

remove_piece(Board, X, Y, BoardOut) :-
    nth0(Y, Board, YList),
    nth0(X, YList, Item),
    replace_nth0(YList, X, Item, '.', NewYList),
    replace_nth0(Board, Y, YList, NewYList, BoardOut).

capture(Board,X1,Y1,X2,Y2,BoardOut) :-
    XDiff is (X2 - X1)/2 + X1,
    YDiff is (Y2 - Y1)/2 + Y1,
    remove_piece(Board, XDiff, YDiff, NewBoard),
    nth0(Y1, Board, YList),
    nth0(X1, YList, PosSymbol),
    move(PosSymbol, X1, Y1, X2, Y2, NewBoard, BoardOut).

is_corresponding_player(PlayerNumber, Board, X, Y) :-
    nth0(Y, Board, YList),
    nth0(X, YList, Item),
    player_number(Item, PlayerNumber).

checkers_move(Board, X1, Y1, X2, Y2, OutputBoard) :-
    nth0(Y1, Board, YList),
    nth0(X1, YList, PosSymbol),
    is_legal_move(Board, PosSymbol, X1, Y1, X2, Y2),
    move(PosSymbol, X1, Y1, X2, Y2, Board, OutputBoard).

piece_legal_moves(Board, X, Y, PieceLegalMoves) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Piece),
    Piece \= '.',
    piece_color(Piece, Color),
    (
        Color == white -> 
            Directions = [(-1, -1), (1, -1)]
        ; 
        Color == black -> 
            Directions = [(-1, 1), (1, 1)]
    ),
    findall([NewX, NewY],
        (
            member((DX, DY), Directions),
            NewX is X + DX,
            NewY is Y + DY,
            valid_coordinate(8, NewX, NewY),
            nth0(NewY, Board, NewRow),
            nth0(NewX, NewRow, TargetPiece),
            TargetPiece == '.'
        ),
        PieceLegalMoves
    ).

player_legal_moves(Board, Board_size, PlayerColor, LegalMoves, Forced) :-
    player_forced_moves(Board, Board_size, PlayerColor, PlayerForcedMoves),
    PlayerForcedMoves \= [] ->
        LegalMoves = PlayerForcedMoves,
        Forced = 1
    ;
        (
            MaxIdx is Board_size - 1,
            findall(
                [X, Y, Moves],
                (
                    between(0, MaxIdx, X),
                    between(0, MaxIdx, Y),
                    nth0(Y, Board, Row),
                    nth0(X, Row, Piece),
                    piece_color(Piece, PlayerColor),
                    piece_legal_moves(Board, X, Y, TempMoves),
                    TempMoves \= [],
                    findall(
                        [XM, YM],
                        member([XM, YM], TempMoves),
                        Moves
                    )
                ),
                LegalMoves
            ),
            Forced = 0
        ).

play(Board, Board_size, PlayerSymbol, LastX, LastY, 1) :-
    
    check_game_over(Board, Board_size, PlayerSymbol),

    % console prints
    print_checkers(Board, Board_size),
    player_number(PlayerSymbol, PlayerNumber),

    % read inputs
    (
        read_input(Board_size,PlayerNumber, X1, Y1, X2, Y2) -> 
            true 
        ;
        write("Incorrect input format, try again."), nl,
        sleep(2),
        play(Board, Board_size, PlayerSymbol, LastX, LastY, 1)
    ),

    % input validation
    (   
        (
            \+ valid_coordinate(Board_size, X1, Y1);
            \+ valid_coordinate(Board_size, X2, Y2);
            \+ is_corresponding_player(PlayerNumber, Board, X1, Y1)
        ) ->
            write("Invalid input. Try again..."), nl,
            sleep(2),
            play(Board, Board_size, PlayerSymbol, LastX, LastY, 1), !
        ;

        % REGULAR MOVE PLAY
        (
            LastX =:= Board_size ->

            % verify forced moves
            piece_color(PlayerSymbol, Color),
            player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),

            (
                PlayerForcedMoves \= [] ->

                % check if given move is contained in forced moves list
                (
                    \+ is_move_forced_valid(X1, Y1, X2, Y2, PlayerForcedMoves) ->
                        write("Invalid move! Hint: You must capture a piece when able."), nl,
                        sleep(2),
                        play(Board, Board_size, PlayerSymbol, LastX, LastY, 1), !
                ;

                    % perform capture
                    capture(Board, X1, Y1, X2, Y2, AfterMoveBoard),

                    % check if it's a COMBO move
                    has_forced_move(AfterMoveBoard, Board_size, X2, Y2, ComboForcedMove),
                    (
                        ComboForcedMove \= [] ->
                            play(AfterMoveBoard, Board_size, PlayerSymbol, X2, Y2, 1)
                    ;
                        opponent_color(PlayerSymbol, OppColor),
                        %minimax(AfterMoveBoard, Board_size, OppColor, BoardBest, _),
                        alphabeta(AfterMoveBoard, Board_size, OppColor, OppColor, 3, -9999, 9999, GoodBoard, _),
                        play(GoodBoard, Board_size, PlayerSymbol, Board_size, Board_size, 1)
                    )
                )

                ;
                
                % no forced moves - proceed with regular move
                checkers_move(Board, X1, Y1, X2, Y2, MovedBoard),
                opponent_color(PlayerSymbol, OppColor),
                (
                    %minimax(MovedBoard, Board_size, OppColor, BoardBest, _)
                    alphabeta(MovedBoard, Board_size, OppColor, OppColor, 3, -9999, 9999, GoodBoard, _) ->
                        play(GoodBoard, Board_size, PlayerSymbol, Board_size, Board_size, 1)
                ),
                play(MovedBoard, Board_size, PlayerSymbol, Board_size, Board_size, 1)
            )

            ;

            % COMBO MOVE

            % valid move verification
            (
                % error on this condition
                (
                    X1 =\= LastX; 
                    Y1 =\= LastY
                ) ->
                    write("Invalid input. Try again..."), nl,
                    sleep(2),
                    play(Board, Board_size, PlayerSymbol, LastX, LastY, 1), !
                ;

                % check existing forced moves
                has_forced_move(Board, Board_size, LastX, LastY, ForcedMoveList),
                (
                    forced_moves_contains(X2, Y2, ForcedMoveList) ->
                        % perform capture
                        capture(Board, X1, Y1, X2, Y2, CapturedBoard),
                        % check if there's another forced move
                        has_forced_move(CapturedBoard, Board_size, X2, Y2, ComboForcedMoves),
                        (
                            ComboForcedMoves \= [] ->
                                play(CapturedBoard, Board_size, PlayerSymbol, X2, Y2, 1)
                        ;
                            opponent_symbol(PlayerSymbol, Opp),
                            play(CapturedBoard, Board_size, Opp, Board_size, Board_size, 1)
                        )
                ;
                    write("Invalid move! Hint: You must capture a piece when able."), nl,
                    sleep(2),
                    play(Board, Board_size, PlayerSymbol, LastX, LastY, 1), !
                )
            )
        )
    ).

play(Board, Board_size, PlayerSymbol, LastX, LastY, 0) :-

    check_game_over(Board, Board_size, PlayerSymbol),
    % console prints
    print_checkers(Board, Board_size),
    player_number(PlayerSymbol, PlayerNumber),

    % read inputs
    (
        read_input(Board_size, PlayerNumber, X1, Y1, X2, Y2) -> 
            true 
        ;
        write("Incorrect input format, try again."), nl,
        sleep(2),
        play(Board, Board_size, PlayerSymbol, LastX, LastY, 0), !
    ),

    % input validation
    (   
        (
            \+ valid_coordinate(Board_size, X1, Y1);
            \+ valid_coordinate(Board_size, X2, Y2);
            \+ is_corresponding_player(PlayerNumber, Board, X1, Y1)
        ) ->
            write("Invalid input. Try again..."), nl,
            sleep(2),
            play(Board, Board_size, PlayerSymbol, LastX, LastY, 0), !
        ;

        % REGULAR MOVE PLAY
        (
            LastX =:= Board_size ->

            % verify forced moves
            piece_color(PlayerSymbol, Color),
            player_forced_moves(Board, Board_size, Color, PlayerForcedMoves),

            (
                PlayerForcedMoves \= [] ->

                % check if given move is contained in forced moves list
                (
                    \+ is_move_forced_valid(X1, Y1, X2, Y2, PlayerForcedMoves) ->
                        write("Invalid move! Hint: You must capture a piece when able."), nl,
                        sleep(2),
                        play(Board, Board_size, PlayerSymbol, LastX, LastY, 0), !
                ;

                    % perform capture
                    capture(Board, X1, Y1, X2, Y2, AfterMoveBoard),

                    % check if it's a COMBO move
                    has_forced_move(AfterMoveBoard, Board_size, X2, Y2, ComboForcedMove),
                    (
                        ComboForcedMove \= [] ->
                            play(AfterMoveBoard, Board_size, PlayerSymbol, X2, Y2, 0)
                    ;
                        opponent_symbol(PlayerSymbol, Opp),
                        play(AfterMoveBoard, Board_size, Opp, Board_size, Board_size, 0)
                    )
                )

                ;
                
                % no forced moves - proceed with regular move
                checkers_move(Board, X1, Y1, X2, Y2, MovedBoard),
                opponent_symbol(PlayerSymbol, Opp),
                play(MovedBoard, Board_size, Opp, Board_size, Board_size, 0)
            )

            ;

            % COMBO MOVE

            % valid move verification
            (
                % error on this condition
                (
                    X1 =\= LastX; 
                    Y1 =\= LastY
                ) ->
                    write("Invalid input. Try again..."), nl,
                    sleep(2),
                    play(Board, Board_size, PlayerSymbol, LastX, LastY, 0), !
                ;

                % check existing forced moves
                has_forced_move(Board, Board_size, LastX, LastY, ForcedMoveList),
                (
                    forced_moves_contains(X2, Y2, ForcedMoveList) ->
                        % perform capture
                        capture(Board, X1, Y1, X2, Y2, CapturedBoard),
                        % check if there's another forced move
                        has_forced_move(CapturedBoard, Board_size, X2, Y2, ComboForcedMoves),
                        (
                            ComboForcedMoves \= [] ->
                                play(CapturedBoard, Board_size, PlayerSymbol, X2, Y2, 0)
                        ;
                            opponent_symbol(PlayerSymbol, Opp),
                            play(CapturedBoard, Board_size, Opp, Board_size, Board_size, 0)
                        )
                ;
                    write("Invalid move! Hint: You must capture a piece when able."), nl,
                    sleep(2),
                    play(Board, Board_size, PlayerSymbol, LastX, LastY, 0), !
                )
            )
        )
    ).

checkers(Board_size, Bot) :-

    write("Welcome to the Checkers Prolog game!"), nl,

    % base board preparation
    generate_empty_board(InitialBoard, Board_size),
    Player_rows is (Board_size-2) // 2,
    fill_board(InitialBoard, Player_rows, 0, FilledBoard),

    % VERIFY IF PLAYING WITH BOT

    play(FilledBoard, Board_size, '\u25cb', Board_size, Board_size, Bot).

chain_captures(Board, Board_size, X, Y, _, [Board]) :-
    has_forced_move(Board, Board_size, X, Y, ForcedMoves),
    ForcedMoves == [].

chain_captures(Board, Board_size, X, Y, PlayerSymbol, FinalBoards) :-
    has_forced_move(Board, Board_size, X, Y, ForcedMoves),
    ForcedMoves \= [],
    findall(
        FinalBoard,
        (
            member([XF, YF], ForcedMoves),
            capture(Board, X, Y, XF, YF, TempBoard),
            chain_captures(TempBoard, Board_size, XF, YF, PlayerSymbol, Continuations),
            member(FinalBoard, Continuations)
        ),
        FinalBoards
    ).

generate_boards_from_moves(_, [], []).

generate_boards_from_moves(Board, [[XI, YI, Moves] | Tail], [MovedBoards | Boards]) :-
    findall(
        NewBoard,
        (
            member([XF, YF], Moves),
            nth0(YI, Board, YList),
            nth0(XI, YList, PlayerSymbol),
            move(PlayerSymbol, XI, YI, XF, YF, Board, NewBoard)
        ),
        MovedBoards
    ),
    generate_boards_from_moves(Board, Tail, Boards).

generate_boards_from_moves(_, _, [], []).

generate_boards_from_moves(Board, Board_size, [[XI, YI, Moves] | Tail], [AllChains | Boards]) :-
    nth0(YI, Board, YList),
    nth0(XI, YList, PlayerSymbol),
    findall(
        FinalBoard,
        (
            member([XF, YF], Moves),
            capture(Board, XI, YI, XF, YF, TempBoard),
            chain_captures(TempBoard, Board_size, XF, YF, PlayerSymbol, Continuations),
            member(FinalBoard, Continuations)
        ),
        AllChains
    ),
    generate_boards_from_moves(Board, Board_size, Tail, Boards).
    
moves(BoardIn, Board_size, Player_color, Return) :-
    player_legal_moves(BoardIn, Board_size, Player_color, LegalMoves, Forced),
    count_color(BoardIn, Player_color, N),
    opponent_color(Player_color, OppColor),
    count_color(BoardIn, OppColor, OppN),
    (
        LegalMoves \= [],
        OppN \= 0,
        N \= 0 ->
        ( 
            Forced =:= 1 ->
                adapt_moves(LegalMoves, ForcedLegalMoves),
                generate_boards_from_moves(BoardIn, Board_size, ForcedLegalMoves, ForcedMovesBoardsTemp), !,
                findall(
                    Board,
                    (
                        member(Boards, ForcedMovesBoardsTemp),
                        member(Board, Boards)
                    ),
                    Return
                )
            ;
            generate_boards_from_moves(BoardIn, LegalMoves, LegalMovesBoardsTemp), !,
            findall(
                Board,
                (
                    member(Boards, LegalMovesBoardsTemp),
                    member(Board, Boards)
                ),
                Return
            )
        )
        ;
        fail
    ).

minimax(BoardIn, Board_size, Player_color, BestSucc, Val) :-
    moves(BoardIn, Board_size, Player_color, PosList), !,
    best( PosList, Board_size, Player_color, BestSucc, Val)
    ;
    staticval(Player_color,BoardIn, Val).

best( [Board], Board_size, Player_color, Board, Val) :-
    opponent_color(Player_color, OppColor),
    minimax( Board, Board_size, OppColor, _, Val), !.

best( [Board1 | BoardList ], Board_size, Player_color, BestBoard, BestVal ) :-
    opponent_color(Player_color, OppColor),
    minimax( Board1, Board_size, OppColor, _, Val1),
    best(BoardList, Board_size, Player_color, Board2, Val2),
    betterof(Player_color, Board1, Val1, Board2, Val2, BestBoard, BestVal).

betterof(Player_color, Board0, Val0, _, Val1, Board0, Val0) :-
    Player_color == white ->
        Val0 < Val1, !;
    Val1 < Val0, !.

betterof( _, _, _, Board1, Val1, Board1, Val1).

staticval(Player_color, Board, Val) :-
    Player_color == white ->  
        count_pieces(Board, '\u25cb', WhiteNumber),
        count_pieces(Board, '\u25cf', BlackNumber),
        Val is WhiteNumber -  BlackNumber
    ;
    count_pieces(Board, '\u25cb', WhiteNumber),
    count_pieces(Board, '\u25cf', BlackNumber),
    Val is BlackNumber - WhiteNumber.

count_pieces([], _, 0).

count_pieces([Row | Rest], Piece, Count) :-
    count_row_pieces(Row, Piece, RowCount),
    count_pieces(Rest, Piece, RestCount),
    Count is RowCount + RestCount.

count_row_pieces([], _, 0).

count_row_pieces([Piece | Rest], Piece, Count) :-
    count_row_pieces(Rest, Piece, RestCount),
    Count is RestCount + 1.

count_row_pieces([Other | Rest], Piece, Count) :-
    Piece \== Other,
    count_row_pieces(Rest, Piece, Count).

count_color(Board, white, N) :-
    count_pieces(Board, '\u25cb', WhiteCount),
    count_pieces(Board, '\u2655', WhiteQueenCount),
    N is WhiteCount +  WhiteQueenCount.

count_color(Board, black, N) :-
    count_pieces(Board, '\u25cf', BlackCount),
    count_pieces(Board, '\u265B', BlackQueenCount),
    N is BlackCount +  BlackQueenCount.

adapt_moves([], []).

adapt_moves([[XI, YI | MovesTail] | Tail], [[XI, YI, MovesTail] | TailForcedMoves]) :-
    adapt_moves(Tail, TailForcedMoves).

check_game_over(Board, Board_size, PlayerSymbol) :-
    piece_color(PlayerSymbol, Color),
    count_color(Board, Color, N),
    opponent_color(Color, OppColor),
    (  
        N =:= 0 ->
            opponent_symbol(PlayerSymbol, OppSym),
            piece_color(OppSym, OppColor),
            format("Player ~w has no pieces Player ~w wins!~n", [Color, OppColor]), nl,
            print_checkers(Board, Board_size), !,
            halt
        ;  % Empate por falta de legal moves
        player_legal_moves(Board, Board_size, Color, LegalMoves, _),
        LegalMoves == [] ->
            format("Player ~w has no legal moves Player ~w wins!~n", [Color, OppColor]), nl,
            print_checkers(Board, Board_size), !,
            halt
        ;
        true
    ).

alphabeta( Board, Board_size, PlayerColor, PlayerMax, Depth, Alpha, Beta, GoodBoard, Val) :-
    (
        Depth >= 0 ->
            moves(Board, Board_size, PlayerColor, BoardList), !,
            NewDepth is Depth - 1,
            boundedbest( BoardList, Board_size, PlayerColor, PlayerMax, NewDepth, Alpha, Beta, GoodBoard, Val)
            ; % Or                       
            staticval(PlayerColor, PlayerMax, Board, Val)       % Static value of Board
    ).
    
boundedbest( [Board | BoardList], Board_size, PlayerColor, PlayerMax, Depth, Alpha, Beta, GoodBoard, GoodVal) :-
    opponent_color(PlayerColor, OppColor),
    alphabeta( Board, Board_size, OppColor, PlayerMax, Depth, Alpha, Beta, _, Val),
	goodenough( BoardList, Board_size, PlayerColor, PlayerMax, Depth, Alpha, Beta, Board, Val, GoodBoard, GoodVal).

goodenough( [], _, _, _, _, _, _, Board, Val, Board, Val) :- !.     % No other candidate

goodenough( _, _, PlayerColor, PlayerMax, _, Alpha, Beta, Board, Val, Board, Val) :-
	min_to_move( PlayerColor, PlayerMax), Val > Beta, !   % Maximizer attained upper bound
	; % Or
	max_to_move( PlayerColor, PlayerMax), Val < Alpha, !. % Minimizer attained lower bound

goodenough( BoardList, Board_size, PlayerColor, PlayerMax, Depth, Alpha, Beta, Board, Val, GoodBoard, GoodVal) :-
    newbounds( PlayerColor, PlayerMax, Alpha, Beta, Board, Val, NewAlpha, NewBeta),
	boundedbest( BoardList, Board_size, PlayerColor, PlayerMax, Depth, NewAlpha, NewBeta, Board1, Val1),
	betterof( PlayerColor, PlayerMax, Board, Val, Board1, Val1, GoodBoard, GoodVal).      % Refine bounds

newbounds( PlayerColor, PlayerMax, Alpha, Beta, _, Val, Val, Beta) :-
	min_to_move( PlayerColor, PlayerMax), Val > Alpha, !.         % Maximizer increased lower bound

newbounds( PlayerColor, PlayerMax, Alpha, Beta, _, Val, Alpha, Val) :-
	max_to_move( PlayerColor, PlayerMax), Val < Beta, !.          % Minimizer decreased upper bound
	
newbounds(_, _, Alpha, Beta, _, _, Alpha, Beta).   % Otherwise bounds unchanged

betterof( PlayerColor, PlayerMax, Board, Val, _, Val1, Board, Val) :-   % Board better than Board1
	min_to_move( PlayerColor, PlayerMax), Val > Val1, !
	; % Or
	max_to_move( PlayerColor, PlayerMax), Val < Val1, !.

betterof( _, _, _, _, Board1, Val1, Board1, Val1). % Otherwise Board1 better

min_to_move(PlayerColor, PlayerMax):- PlayerColor == PlayerMax.

max_to_move(PlayerColor, PlayerMax):- PlayerColor \= PlayerMax.

staticval(PlayerColor, PlayerMax, Board, Val) :-
    count_pieces(Board, '\u25cb', WhiteNumber),
    count_pieces(Board, '\u25cf', BlackNumber),
    (  
        PlayerMax == PlayerColor, PlayerColor == white ->
            Val is WhiteNumber - BlackNumber
        ; 
        PlayerMax == PlayerColor, PlayerColor == black ->
            Val is BlackNumber - WhiteNumber
        ;  
        PlayerColor == white ->
            Val is BlackNumber - WhiteNumber
        ;  
        PlayerColor == black ->
            Val is WhiteNumber - BlackNumber
    ).
    