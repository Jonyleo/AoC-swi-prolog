:- use_module(library(clpfd)).

main :-
    open('test.txt', read, Str),
    read_line(Str,Rolls),
    get_code(Str, _),
    read_boards(Str, Boards),
    close(Str), !,
    maplist(empty_board, Boards, BI),
    get_first_winner(Boards, BI, Winner, WinnerI, Rolls, Roll),
    write(Winner), nl, !,
    get_unmarked(Winner, WinnerI, Sum),
    atom_number(Roll, Roll_int),
    Res is Sum * Roll_int,
    write(Res),
    maplist(full_board, Boards, BF),
    get_first_loser(Boards, BF, Loser, LoserI, Rolls, RollL),
    write(Loser), nl, !,
    apply_number(Loser, LoserI, LoserIF, RollL),
    get_unmarked(Loser, LoserIF, SumL),
    atom_number(RollL, RollL_int),
    Res1 is SumL * RollL_int,
    write(SumL), nl, write(RollL_int), nl,
    write(Res1).

get_unmarked([], [], 0).

get_unmarked([H | T], [H1 | T1], Res) :-
    get_unmarked_line(H, H1, R),
    get_unmarked(T, T1, R1),
    Res is R + R1.

get_unmarked_line([], [], 0).

get_unmarked_line([H | T], [0 | T1], Res):- !,
    get_unmarked_line(T, T1, Res_T),
    atom_number(H, H_int),
    Res is Res_T + H_int.

get_unmarked_line([_ | T], [1 | T1], Res):-
    get_unmarked_line(T, T1, Res).



get_first_winner(Boards, BoardsI, Board, BoardI, Rolls, Roll) :-
    get_first_winner_aux(Boards, BoardsI, Board, BoardI, Rolls, Roll, 0).

get_first_winner_aux(Boards, BoardsI, Board, BoardI, _, Roll, Roll) :-
    get_winner(Boards, BoardsI, Board, BoardI).

get_first_winner_aux(Boards, BoardsI, Board, BoardI, [H | T], Roll, _) :-
    apply_all(Boards, BoardsI, BoardsNI, H),
    get_first_winner_aux(Boards, BoardsNI, Board, BoardI, T, Roll, H).



get_first_loser(Boards, BoardsI, Board, BoardI, Rolls, Roll) :-
    reverse(Rolls, Rolls_rev),
    get_first_loser_aux(Boards, BoardsI, Board, BoardI, Rolls_rev, Roll, 0).

get_first_loser_aux(Boards, BoardsI, Board, BoardI, _, Roll, Roll) :-
    get_loser(Boards, BoardsI, Board, BoardI).

get_first_loser_aux(Boards, BoardsI, Board, BoardI, [H| T], Roll, _) :-
    unapply_all(Boards, BoardsI, BoardsNI, H),
    get_first_loser_aux(Boards, BoardsNI, Board, BoardI, T, Roll, H).


get_winner([H | _], [H1 | _], H, H1) :-
    is_winner(H1), !.

get_winner([_ | T], [_ | T1], Board, BoardI) :-
    get_winner(T, T1, Board, BoardI).

get_loser([H | _], [H1 | _], H, H1) :-
    \+ is_winner(H1), !.

get_loser([_ | T], [_ | T1], Board, BoardI) :-
    get_loser(T, T1, Board, BoardI).



empty_board(_, BoardI) :-
    BoardI = [[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]].

full_board(_, BoardI) :-
    BoardI = [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]].

unapply_all([],[],[],_).

unapply_all([B | T], [BI | TI], [BIN | TIN], Number) :-
    unapply_number(B, BI, BIN, Number),
    unapply_all(T, TI, TIN, Number).

unapply_number([], [], [], _).

unapply_number([[Number|T] | TT], [[1|TO] | TTO], [[0|TN] | TTN] , Number) :- !,
    unapply_number([T | TT], [TO | TTO], [TN | TTN], Number).

unapply_number([[_|T] | TT], [[HO|TO] | TTO], [[HO|TN] | TTN] , Number) :- !,
    unapply_number([T | TT], [TO | TTO], [TN | TTN], Number).

unapply_number([[] | TT], [[] | TTO], [[] | TTN] , Number) :- !,
    unapply_number(TT, TTO, TTN, Number).

apply_all([],[],[],_).

apply_all([B | T], [BI | TI], [BIN | TIN], Number) :-
    apply_number(B, BI, BIN, Number),
    apply_all(T, TI, TIN, Number).

apply_number([], [], [], _).

apply_number([[Number|T] | TT], [[0|TO] | TTO], [[1|TN] | TTN] , Number) :- !,
    apply_number([T | TT], [TO | TTO], [TN | TTN], Number).

apply_number([[_|T] | TT], [[HO|TO] | TTO], [[HO|TN] | TTN] , Number) :- !,
    apply_number([T | TT], [TO | TTO], [TN | TTN], Number).

apply_number([[] | TT], [[] | TTO], [[] | TTN] , Number) :- !,
    apply_number(TT, TTO, TTN, Number).

is_winner(Board) :-
    is_winner_line(Board).
is_winner(Board) :-
    is_winner_col(Board).

is_winner_line([L | _]) :- 
    maplist(=(1), L).
    

is_winner_line([_ | T]) :-
    is_winner_line(T).

is_winner_col(Board) :-
    transpose(Board, Board_T),
    is_winner_line(Board_T).


/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/

read_line(Stream, [W| L]) :-
    \+ at_end_of_stream(Stream), 
    \+ peek_code(Stream, 10), !,
    readWord(Stream, W),
    read_line(Stream, L).

read_line(_, []).


read_boards(Stream,[]) :-
    at_end_of_stream(Stream), !.

read_boards(Stream,[W|L]) :-
    \+ at_end_of_stream(Stream),
    \+ peek_code(Stream, -1), !,
    read_board(Stream,W),
    get_code(Stream, _),
    read_boards(Stream,L).

read_board(Stream, [L | R]) :-
    \+ at_end_of_stream(Stream), 
    \+ peek_code(Stream, 10), !,
    read_five(Stream, L),
    read_board(Stream, R).

read_board(_, []).


five(Stream, W) :- between(0, 4, _), readWord(Stream, W).


read_five(Stream, Ws) :-

    findall(W, five(Stream, W), Ws).

/*
    
    Code addapted from
    http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse54

*/

readWord(InStream,_):-
        ignore_spaces(InStream).

readWord(InStream, W) :- !,
         get_code(InStream,Char),
         checkCharAndReadRest(Char,Chars,InStream),
         atom_codes(W,Chars).

checkCharAndReadRest(44,[],_):-  !.
   
checkCharAndReadRest(32,[],InStream):- ignore_spaces(InStream).
checkCharAndReadRest(32,[],_):- !.

checkCharAndReadRest(10,[],_):-  !.

checkCharAndReadRest(-1,[],_):-  !.

checkCharAndReadRest(end_of_file,[],_):-  !.

checkCharAndReadRest(Char,[Char|Chars],InStream):-
     get_code(InStream,NextChar),
     checkCharAndReadRest(NextChar,Chars,InStream). 

ignore_spaces(InStream) :-
    peek_code(InStream, 32),
    get_code(InStream,_),
    ignore_spaces(InStream).