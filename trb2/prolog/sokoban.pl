
:- use_module('definitions').
:- use_module('prints').

example_level_1(
    [
        ['#','#','#','#','#','#'],
        ['#', '@', '$', '.', '#'],
        ['#','#','#','#','#','#']
    ]
).

example_level_2(
    [
        ['#','#','#','#','#','#'],
        ['#',' ','$',' ','.','#'],
        ['#','@',' ',' ',' ','#'],
        ['#','#','#','#','#','#']
    ]
).

example_level_3(
    [
        ['#','#','#','#','#','#','#','#'],
        ['#','#','#',' ',' ',' ','#','#'],
        ['#',' ','@','$',' ','.','#','#'],
        ['#','#','#',' ','$','.','#','#'],
        ['#','#','#','#','#','#','#','#']
    ]
).

example_level_4(
    [
        ['#','#','#','#','#','#','#','#'],
        ['#','#','#',' ',' ',' ','#','#'],
        ['#','?','%','@','$','.','#','#'],
        ['#','#','#',' ',' ',' ','#','#'],
        ['#',' ',' ',' ','$','.','#','#'],
        ['#',' ',' ','$',' ','.','#','#'],
        ['#','#','#','#','#','#','#','#']
    ]
).

example_level_5(
    [
        ['#','#','#','#','#','#','#','#'],
        ['#','#','#',' ',' ',' ','#','#'],
        ['#','.','@','$',' ',' ','#','#'],
        ['#','#','#',' ','$','.','#','#'],
        ['#','.','#','#','$',' ','#','#'],
        ['#',' ','#',' ','.',' ','#','#'],
        ['#','$',' ',' ','$','$','.','#'],
        ['#',' ',' ',' ','.',' ',' ','#'],
        ['#','#','#','#','#','#','#','#']
    ]
).

/*
** Row = Y, Col = X
** X = Col, Y = Row
*/

find_player_in_row([Tile | _], CurrentColIndex, CurrentColIndex) :-
    player_symbol(Tile).

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
    box_symbol(Tile),
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
        player_symbol(Tile) = player_symbol('@'),
        replace_tile(OldBoard, X, Y, ' ', AfterBoard)
    ;
        player_symbol(Tile) = player_symbol('+'),
        replace_tile(OldBoard, X, Y, '.', AfterBoard)
    ),
    get_board(AfterBoard, Rest, NewBoard).

get_board(OldBoard, [box_at(X, Y) | Rest], NewBoard) :-
    get_tile(OldBoard, X, Y, Tile),
    (
        box_symbol(Tile) = box_symbol('$'),
        replace_tile(OldBoard, X, Y, ' ', AfterBoard)
    ;
        box_symbol(Tile) = box_symbol('*'),
        replace_tile(OldBoard, X, Y, '.', AfterBoard)
    ),
    get_board(AfterBoard, Rest, NewBoard).

assert_board_in_row([], _, _).

assert_board_in_row([Tile | RestTiles], Row, Col) :-
    (
        wall_symbol(Tile),
        assert(wall_at(Col, Row))
    ;
        goal_symbol(Tile),
        assert(goal_at(Col, Row))
    ;
        gate_symbol(Tile),
        assert(gate_at(Col, Row))
    ;
        final_goal_symbol(Tile),
        assert(final_goal_at(Col, Row))
    ),
    NextCol is Col + 1,
    assert_board_in_row(RestTiles, Row, NextCol).

assert_board_in_row([_ | RestTiles], Row, Col) :-
    NextCol is Col + 1,
    assert_board_in_row(RestTiles, Row, NextCol).

assert_board([], _).

assert_board([BoardHead | BoardTail], CurrRow) :-
    assert_board_in_row(BoardHead, CurrRow, 0),          
    NextRow is CurrRow + 1,
    assert_board(BoardTail, NextRow).

%            In       Out  
map_loader(GameMap, State) :-
    find_player(GameMap, Col, Row),
    find_boxes(GameMap, 0, Boxes),
    get_state(player_at(Row, Col), Boxes, State),
    get_board(GameMap, State, Board),
    assert_board(Board, 0).

not_member(_, []).

not_member(player_at(PX, PY), [box_at(BX, BY) | RestBoxes]) :-
    dif((PX, PY), (BX, BY)),
    not_member(player_at(PX, PY), RestBoxes).

not_member(box_at(B1X, B1Y), [box_at(B2X, B2Y) | RestBoxes]) :-
    dif((B1X, B1Y), (B2X, B2Y)),
    not_member(box_at(B1X, B1Y), RestBoxes).

m(MoveX, MoveY, [player_at(X, Y) | Boxes], [NewPlayer | NewBoxes]):-
    NewX is X + MoveX,
    NewY is Y + MoveY,
    TempPlayer = player_at(NewX, NewY),
    \+ wall_at(NewX, NewY),
    \+ gate_at(NewX, NewY),
    (
        % Move without pushing box
        not_member(TempPlayer, Boxes),
        NewPlayer = TempPlayer,
        NewBoxes = Boxes
    ;
        % Push box
        \+ not_member(TempPlayer, Boxes),
        BX is NewX + MoveX,
        BY is NewY + MoveY,
        \+ wall_at(BX, BY),
        \+ gate_at(BX, BY),
        NewBox = box_at(BX, BY),
        not_member(NewBox, Boxes),
        select(box_at(NewX, NewY), Boxes, TempBoxes),
        NewBoxes = [ NewBox | TempBoxes],
        NewPlayer = TempPlayer,
        ( 
            goal_at(BX, BY), check_boxes_in_goal(NewBoxes) -> 
            gate_at(XX, YY), write("\n"), write(XX), write(" "), write(YY), write("\n"), write("\nAll boxes in the goals\n"), !
        ; true 
        )
    ).
   
%   In       Out    C (cost)
s(StateIn, StateOut, 1):-
    dir(_, (X, Y)),
    m(X, Y, StateIn, StateOut).

check_boxes_in_goal([]).

check_boxes_in_goal([box_at(X, Y) | RestBoxes]) :-
    goal_at(X, Y),
    check_boxes_in_goal(RestBoxes).

check_player_in_goal(player_at(X, Y)) :-
    final_goal_at(X, Y).

goal([_ | Boxes]) :-
    check_boxes_in_goal(Boxes).

test5:-
    example_level_3(GameMap),
    print_map(GameMap),
    write('\n'),
    %trace,
    map_loader(GameMap, State),
    write('State = '), print_list(State),
    write('\n'),
    write('\n').

test6:-
    example_level_3(GameMap),
    map_loader(GameMap, State),
    write("StateIn "), print_list(State),
    write('\n'),
    %Dir = right,
    s(State, StateOut, 1),
    write("StateOut "), print_list(StateOut),
    write('\n').

test7:-
    example_level_3(GameMap),
    map_loader(GameMap, _),
    wall_at(X, Y),
    print_coordinates(X, Y),
    write('\n').

test8:-
    example_level_4(GameMap),
    map_loader(GameMap, _),
    gate_at(X, Y),
    print_coordinates(X, Y),
    write('\n').

test10:-
    example_level_4(GameMap),
    map_loader(GameMap, _),
    gate_at(X, Y),
    print_coordinates(X, Y),
    retract(gate_at(X, Y)),
    gate_at(-1, -1),
    write("\nIts good\n"),
    write('\n').

