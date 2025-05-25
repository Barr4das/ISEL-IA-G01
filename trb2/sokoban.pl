
:- use_module('definitions').

example_level_1(
    [
        ['#','#','#','#','#'],
        ['#', '@', '$', '.', '#'],
        ['#','#','#','#','#']
    ]
).

example_level_2(
    [
        ['#','#','#','#','#','#','#','#'],
        ['#','#','#',' ',' ',' ','#','#'],
        ['#','#','@','$',' ','.','#','#'],
        ['#','#','#',' ','$','.','#','#']
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

is_row_over([]).
is_row_over([Map_Tile_Head | Map_Tile_Tail]) :-
    \+ is_not_over(Map_Tile_Head),
    is_row_over(Map_Tile_Tail).

goal([]).
goal([Map_Head | Map_Tail]):-
    is_row_over(Map_Head),
    goal(Map_Tail).

bestfirst(Start, Solution) :-
    expand([], l(Start, 0/0), 9999, _, yes, Solution).

find_player([RowHead | _], 0, PlayerCol) :-
    find_player_in_row(RowHead, 0, PlayerCol).

find_player([_ | TailBoard], PlayerRow, PlayerCol) :-
    find_player(TailBoard, TempRow, PlayerCol),
    PlayerRow is TempRow + 1.

find_player_in_row([Tile | _], CurrentColIndex, CurrentColIndex) :-
    player(Tile).

find_player_in_row([_ | TailRow], CurrentColIndex, PlayerCol) :-
    NextColIndex is CurrentColIndex + 1,
    find_player_in_row(TailRow, NextColIndex, PlayerCol).

get_tile(Board, Row, Col, Tile) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Tile).

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

print_coordinates(X, Y) :-
    write("X: "),
    write(X),
    write("\nY: "),
    write(Y).

print_list([]).
print_list([Head | Tail]) :-
    write(Head),
    print_list(Tail).

print_map([]).

print_map([Head | Tail]):-
    print_list(Head),
    write('\n'),
    print_map(Tail).
    

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
