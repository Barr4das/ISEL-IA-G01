
:- use_module('definitions').

example_level_1(
    [
        ['#','#','#','#','#','#'],
        ['#', '@', '$', '.', '#'],
        ['#','#','#','#','#','#']
    ]
).

example_level_2(
    [
        ['#','#','#','#','#','#','#','#'],
        ['#','#','#',' ',' ',' ','#','#'],
        ['#','#','@','$',' ','.','#','#'],
        ['#','#','#',' ','$','.','#','#'],
        ['#','#','#','#','#','#','#','#']
    ]
).

example_level_3(
    [
        ['#','#','#','#','#','#','#','#'],
        ['#','#','#',' ',' ',' ','#','#'],
        ['#','.','@','$',' ',' ','#','#'],
        ['#','#','#',' ','$','.','#','#'],
        ['#','.','#','#','$',' ','#','#'],
        ['#',' ','#',' ','.',' ','#','#'],
        ['#','$',' ','*','$','$','.','#'],
        ['#',' ',' ',' ','.',' ',' ','#'],
        ['#','#','#','#','#','#','#','#']
    ]
).

/*
** Row = Y, Col = X
** X = Col, Y = Row
*/

find_player_in_row([Tile | _], CurrentColIndex, CurrentColIndex) :-
    player(Tile).

find_player_in_row([_ | TailRow], CurrentColIndex, PlayerCol) :-
    NextColIndex is CurrentColIndex + 1,
    find_player_in_row(TailRow, NextColIndex, PlayerCol).

find_player([RowHead | _], 0, PlayerCol) :-
    find_player_in_row(RowHead, 0, PlayerCol).

find_player([_ | TailBoard], PlayerRow, PlayerCol) :-
    find_player(TailBoard, TempRow, PlayerCol),
    PlayerRow is TempRow + 1.

find_boxes_in_row([], _, _, []).

find_boxes_in_row([Tile | RestTiles], Row, Col, [(Row, Col) | BoxCoords]) :-
    box(Tile),
    NextCol is Col + 1,
    find_boxes_in_row(RestTiles, Row, NextCol, BoxCoords).

find_boxes_in_row([_ | RestTiles], Row, Col, BoxCoords) :-
    NextCol is Col + 1,
    find_boxes_in_row(RestTiles, Row, NextCol, BoxCoords).

find_boxes([], _, []).

find_boxes([GameMapHead | GameMapTail], CurrRow, Boxes) :-
    find_boxes_in_row(GameMapHead, CurrRow, 0, SomeBoxes),          
    NextRow is CurrRow + 1,
    find_boxes(GameMapTail, NextRow, RemainingBoxes),        
    append(SomeBoxes, RemainingBoxes, Boxes).

get_boxes_state([], _).

get_boxes_state([], []).

get_boxes_state([(BRow, BCol) | RestBoxes], [box_at(BCol, BRow) | BoxCoords]) :-
    get_boxes_state(RestBoxes, BoxCoords).

get_state(Player, Boxes, [Player | StateBoxes]) :-
    get_boxes_state(Boxes, StateBoxes).  

replace_nth0(Index, List, NewElement, NewList) :-
    length(List, Len),
    Index >= 0,
    Index < Len,
    (   Index = 0 ->
        List = [_|Tail], NewList = [NewElement|Tail]
    ;   List = [Head|Rest],
        NextIndex is Index - 1,
        NewList = [Head|NewTail],
        replace_nth0(NextIndex, Rest, NewElement, NewTail)
    ).

replace_tile(OldBoard, X, Y, NewTile, NewBoard) :-
    nth0(Y, OldBoard, OldY, TempBoard),
    replace_nth0(X, OldY, NewTile, NewY),
    nth0(Y, NewBoard, NewY, TempBoard).

get_tile(Board, X, Y, Tile) :-
    nth0(Y, Board, BoardRow),
    nth0(X, BoardRow, Tile).

get_board(Board, [], Board).

get_board(OldBoard, [player_at(X, Y) | Rest], NewBoard) :-
    get_tile(OldBoard, X, Y, Tile),
    (
        player(Tile) = player('@'),
        replace_tile(OldBoard, X, Y, ' ', AfterBoard)
    ;
        player(Tile) = player('+'),
        replace_tile(OldBoard, X, Y, '.', AfterBoard)
    ),
    get_board(AfterBoard, Rest, NewBoard).

get_board(OldBoard, [box_at(X, Y) | Rest], NewBoard) :-
    get_tile(OldBoard, X, Y, Tile),
    (
        box(Tile) = box('$'),
        replace_tile(OldBoard, X, Y, ' ', AfterBoard)
    ;
        box(Tile) = box('*'),
        replace_tile(OldBoard, X, Y, '.', AfterBoard)
    ),
    get_board(AfterBoard, Rest, NewBoard).

%            In       Out   Out
map_loader(GameMap, State, Board) :-
    find_player(GameMap, Col, Row),
    find_boxes(GameMap, 0, Boxes),
    get_state(player_at(Row, Col), Boxes, State),
    get_board(GameMap, State, Board).

print_coordinates(X, Y) :-
    write("X: "),
    write(X),
    write("\nY: "),
    write(Y).

m(MoveX, MoveY, [player_at(X, Y) | Boxes], [NewPlayer | NewBoxes]):-
    NewX is X + MoveX,
    NewY is Y + MoveY,
    TempPlayer = player_at(NewX, NewY),
    (
        % Move without pushing box
        % Boxes needs to be a list of position not states
        \+ member(NewPlayer, Boxes),
        NewPlayer = TempPlayer,
        NewBoxes = Boxes
    ;
        % Push box
        member(TempPlayer, Boxes),
        B2X is NX + DX,
        B2Y is NY + DY,
        B2 = (B2X, B2Y),
        \+ wall(B2),
        \+ member(B2, Boxes),
        select(TempPlayer, Boxes, RestBoxes),
        NewBoxes = [B2 | RestBoxes],
        NewPlayer = TempPlayer
    ).
   

%   In       Out    C (cost)
s(StateIn, StateOut, 1):-
    dir(_, (X, Y))
    m(X, Y, StateIn, StateOut).


 
/*
find_boxes([GameMapHead | GameMapTail], CurrRow, Boxes) :-
    find_box_in_row(GameMapHead, CurrRow, 0, Box),
    NewBoxes is [Boxes | Box],
    append(Boxes, Box, NewBoxes),
    NewRow is CurrRow + 1.
    find_boxes(GameMapTail, NewRow, NewBoxes). 
*/        

/*
replace_tile(OldBoard, Row, Col, NewTile, NewBoard) :-
    nth0(Row, OldBoard, OldRow, TempBoard),
    replace_nth0(Col, OldRow, NewTile, NewRow),
    nth0(Row, NewBoard, NewRow, TempBoard).

replace_nth0(Index, List, NewElement, NewList) :-
    length(List, Len),
    Index >= 0,
    Index < Len,
    (   Index = 0 ->
        List = [_|Tail], NewList = [NewElement|Tail]
    ;   List = [Head|Rest],
        NextIndex is Index - 1,
        NewList = [Head|NewTail],
        replace_nth0(NextIndex, Rest, NewElement, NewTail)
    ).

get_tile(Board, Row, Col, Tile) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Tile).

can_move_player(CurrentBoard, PRow, PCol, DR, DC, NPRow, NPCol) :-
    NPRow is PRow + DR,
    NPCol is PCol + DC,
    get_tile(CurrentBoard, NPRow, NPCol, NextTile),
    (floor(NextTile) ; objective(NextTile)).

can_push_box(CurrentBoard, PRow, PCol, DR, DC, BRow, BCol, NBRow, NBCol) :-
    BRow is PRow + DR,
    BCol is PCol + DC,
    get_tile(CurrentBoard, BRow, BCol, BoxTile),
    (box(BoxTile) ; score(BoxTile)),
    NBRow is BRow + DR,
    NBCol is BCol + DC,
    get_tile(CurrentBoard, NBRow, NBCol, TargetTile),
    (floor(TargetTile) ; objective(TargetTile)).

update_board_after_move(CurrentBoard, PRow, PCol, NPRow, NPCol, NextBoard) :-
    get_tile(CurrentBoard, PRow, PCol, OldPlayerTile),
    (player(OldPlayerTile) -> OldPlayerChar = ' ' ; OldPlayerChar = '.'),

    get_tile(CurrentBoard, NPRow, NPCol, NewPlayerTile),
    (floor(NewPlayerTile) -> NewPlayerChar = '@' ; NewPlayerChar = '+'),

    replace_tile(CurrentBoard, PRow, PCol, OldPlayerChar, Board1),
    replace_tile(Board1, NPRow, NPCol, NewPlayerChar, NextBoard).

update_board_after_push(CurrentBoard, PRow, PCol, BRow, BCol, NBRow, NBCol, NextBoard) :-
    get_tile(CurrentBoard, PRow, PCol, OldPlayerTile),
    (player(OldPlayerTile) -> OldPlayerChar = ' ' ; OldPlayerChar = '.'),

    get_tile(CurrentBoard, BRow, BCol, OldBoxTile),
    (box(OldBoxTile) -> PlayerOnBoxChar = '@' ; PlayerOnBoxChar = '+'),

    get_tile(CurrentBoard, NBRow, NBCol, TargetTile),
    (floor(TargetTile) -> NewBoxChar = '$' ; NewBoxChar = '*'),

    replace_tile(CurrentBoard, PRow, PCol, OldPlayerChar, Board1),
    replace_tile(Board1, BRow, BCol, PlayerOnBoxChar, Board2),
    replace_tile(Board2, NBRow, NBCol, NewBoxChar, NextBoard).

s(Map, Out, 1) :-
    find_player(Map, PRow, PCol),
    (
        (DR, DC) = (-1, 0), can_push_box(Map, PRow, PCol, DR, DC, BRow, BCol, NBRow, NBCol),
        update_board_after_push(Map, PRow, PCol, BRow, BCol, NBRow, NBCol, Out)
    ;
        (DR, DC) = (1, 0), can_push_box(Map, PRow, PCol, DR, DC, BRow, BCol, NBRow, NBCol),
        update_board_after_push(Map, PRow, PCol, BRow, BCol, NBRow, NBCol, Out)
    ;
        (DR, DC) = (0, -1), can_push_box(Map, PRow, PCol, DR, DC, BRow, BCol, NBRow, NBCol),
        update_board_after_push(Map, PRow, PCol, BRow, BCol, NBRow, NBCol, Out)
    ;
        (DR, DC) = (0, 1), can_push_box(Map, PRow, PCol, DR, DC, BRow, BCol, NBRow, NBCol),
        update_board_after_push(Map, PRow, PCol, BRow, BCol, NBRow, NBCol, Out)
    ;
        (DR, DC) = (-1, 0), can_move_player(Map, PRow, PCol, DR, DC, NPRow, NPCol),
        update_board_after_move(Map, PRow, PCol, NPRow, NPCol, Out)
    ;
        (DR, DC) = (1, 0), can_move_player(Map, PRow, PCol, DR, DC, NPRow, NPCol),
        update_board_after_move(Map, PRow, PCol, NPRow, NPCol, Out)
    ;
        (DR, DC) = (0, -1), can_move_player(Map, PRow, PCol, DR, DC, NPRow, NPCol),
        update_board_after_move(Map, PRow, PCol, NPRow, NPCol, Out)
    ;
        (DR, DC) = (0, 1), can_move_player(Map, PRow, PCol, DR, DC, NPRow, NPCol),
        update_board_after_move(Map, PRow, PCol, NPRow, NPCol, Out)
    ).

is_row_over([]).
is_row_over([Map_Tile_Head | Map_Tile_Tail]) :-
    \+ is_not_over(Map_Tile_Head),
    is_row_over(Map_Tile_Tail).

goal([]).
goal([Map_Head | Map_Tail]):-
    is_row_over(Map_Head),
    goal(Map_Tail).

succlist(_, [], []).

succlist( GO, [N/C | NCs], Ts) :-
	G is GO + C,
	h( N, H),
	F is G + H,
	succlist( GO, NCs, Ts1),
	insert( l(N, F/G), Ts1, Ts).

expand(P, l( N, _), _, _, yes, [N | P]) :-
    goal(N).

expand(P, l(N, F/G), Bound, Tree1, Solved, Sol):-
    F =< Bound,
    (
        bagof( M/C, ( s(N,M,C), \+ member(M, P)), Succ),
        !,
        succlist(G, Succ, Ts),
        bestf(Ts, F1),
        expand(P, t(N, F1/G, Ts), Bound, Tree1, Solved, Sol)
        ;
        Solved = never
    ).

bestfirst(Start, Solution) :-
    expand([], l(Start, 0/0), 9999, _, yes, Solution).

print_coordinates(X, Y) :-
    write("X: "),
    write(X),
    write("\nY: "),
    write(Y).

test4 :-
    example_level_2(Level),
    print_map(Level),
    write('\n'),
    s(Level, LevelOut1, 1),
    print_map(LevelOut1),
    write('\n'),
    s(LevelOut1, LevelOut2, 1),
    print_map(LevelOut2),
    write('\n'),
    s(LevelOut2, LevelOut3, 1),
    print_map(LevelOut3),
    write('\n').
*/


print_list([]).
print_list([Head | Tail]) :-
    write(Head),
    print_list(Tail).
    
print_map([]).

print_map([Head | Tail]):-
    print_list(Head),
    write('\n'),
    print_map(Tail).

test5:-
    example_level_3(GameMap),
    print_map(GameMap),
    write('\n'),
    %trace,
    map_loader(GameMap, State, Board),
    write('State = '), print_list(State),
    write('\n'),
    write('\n'),
    print_map(Board),
    write('\n').

test6:-
    example_level_3(GameMap),
    map_loader(GameMap, State, Board),
    s(State, Board, StateOut, 1),
    print_map(StateOut),
    write('\n').
