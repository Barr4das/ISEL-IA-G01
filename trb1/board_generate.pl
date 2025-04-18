
/*
 * CREATE BOARD STRUCTURE
*/

:- module('board_generate', [
    generate_empty_board/2,
    fill_board/4,
    generate_small_example_board/1
]).

/*
 * generate_empty_board(-Board:List<List<Char>>,+N:UInt)
 * Creates a board, of given size N, with '.' in all positions (representing an empty space)
 * Example:
    generate_empty_board(Board, 2).
    Board = [ ['.', '.'], ['.', '.'] ].
*/
generate_empty_board(Board, N) :-
    length(Board, N), 
    length(Row, N),
    maplist(=('.'), Row),
    maplist(=(Row), Board).

/*
 * add_intervaled_pieces(+PieceSymbol:Any,+[Any, Any | Any], -[Any, Any | Any])
 * Auxiliary function to add_pieces which substitutes the empty odd spaces ('.') in w/ given PieceSymbol.
 * Example:
    add_intervaled_pieces('T', ['.', '.', '.', '.'], Row).
    Row = ['T', '.', '.', '.'].
*/
add_intervaled_pieces(_, [_], _).
add_intervaled_pieces(PieceSymbol, [_, H | T], [PieceSymbol, H | T]).

/*
 * add_pieces(+PiecesSymbol:Any,+[Any, Any | Any],-[Any, Any | Any])
 * Auxiliary function to fill_board which adds given player pieces intervaled in rows.
 * Example:
    add_pieces('T', [['.', '.'], ['.','.']], Result).
    Result = ['T', ['.', '.']|_].
*/
add_pieces(_, [], _).
add_pieces(_, [_], [_]).

add_pieces(PieceSymbol, [HI, HI2 | TI], [HO, HI2 | Rest]) :-
    add_intervaled_pieces(PieceSymbol, [HI, HI2 | TI], [HO, HI2 | _]),
    add_pieces(PieceSymbol, TI, Rest).

/*
 * fill_board(+[Any | Any], +Player_rows:UInt, +Idx:UInt, -[Any | Any])
 * Generates the starting state of the checkers game.
 * Example:
    fill_board([['.','.'],['.','.']], 1, 2, Result). 
    Result = [['.', '.'], ['.', ○]] .
*/
fill_board([], _, _, []).

fill_board([HI | TI], Player_rows, Idx, [HO | TO]) :-
    Idx < Player_rows, !,
    New_idx is Idx + 1,
    process_row(HI, '\u25cf', Idx, HO),
    fill_board(TI, Player_rows, New_idx, TO).

fill_board([HI | TI], Player_rows, Idx, [HO | TO]) :-
    Idx >= Player_rows + 2, !,
    New_idx is Idx + 1,
    process_row(HI, '\u25cb', Idx, HO),
    fill_board(TI, Player_rows, New_idx, TO).

fill_board([HI | TI], Player_rows, Idx, [HI | TO]) :-
    New_idx is Idx + 1,
    fill_board(TI, Player_rows, New_idx, TO).

/**
 * process_row(+Row:List<Char>, +Piece:Char, +Idx:UInt, -Result:List<Char>)
 * Auxiliary function to ease the add pieces process to rows with inverted positioning
*/
process_row(Row, Piece, Idx, Result) :-
    (
        1 =:= Idx mod 2 ->  
            reverse(Row, RevRow),
            add_pieces(Piece, RevRow, RevResult),
            reverse(RevResult, Result)
        ;   
        add_pieces(Piece, Row, Result)
    ).

/**
 * generate_small_example_board(+Board:List<List<Any>>)
 * Generates a 4 by 4 board with initialized pieces (non functional game state)
*/
generate_small_example_board(Board) :-
    Board = [
        [ '\u25cf', '.', '\u25cf', '.' ],
        [ '.' , '.', '.', '.'],
        [ '.' , '.', '.', '.'],
        [ '.','\u25cb' , '.', '\u25cb' ]
    ].
