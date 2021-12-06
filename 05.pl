main :-
    open('test.txt', read, Str),
    read_file(Str,Vents),
    close(Str), !,
    get_max_x(Vents, X_Max),
    get_max_y(Vents, Y_Max), 

    make_matrix(X_Max, Y_Max, Floor), 
    include(is_line, Vents, Vents_Line),
    render_lines(Vents_Line, Floor),
    count_danger(Floor, C),
    write(C), nl,

    make_matrix(X_Max, Y_Max, Floor1), 
    render_lines(Vents, Floor1),
    count_danger(Floor1, C1),
    write(C1).

count_danger([], 0).
count_danger([H | T], C) :-
    count_danger(T, C1),
    count_danger_line(H, CL),
    C is C1 + CL.

count_danger_line([], 0).
count_danger_line([H | T], C) :-
    depth(H, N),
    N >= 2, !,
    count_danger_line(T, C1),
    C is C1 + 1.

count_danger_line([_ | T], C) :-
    count_danger_line(T, C).

render_lines([], _).

render_lines([H | T], M) :-
    render_line(H, M),
    render_lines(T, M).

render_line([[X, Y1], [X, Y2]], M) :- !,
    YS is min(Y1, Y2),
    YE is max(Y1, Y2),
    render_line_vert(X, YS, YE, M).

render_line([[X1, Y], [X2, Y]], M) :- !,
    XS is min(X1, X2),
    XE is max(X1, X2),
    render_line_diag(Y, XS, XE, M, 0).

render_line([[X1, Y1], [X2, Y2]], M) :-
    X1 =< X2, !,
    Inc is (Y2 - Y1) / abs(Y2 - Y1),
    render_line_diag(Y1, X1, X2, M, Inc).

render_line([[X1, Y1], [X2, Y2]], M) :-
    X1 > X2, !,
    Inc is (Y1 - Y2) / abs(Y1 - Y2),
    render_line_diag(Y2, X2, X1, M, Inc).

render_line_vert(X, Y1, Y2, M) :-
    Y1 =< Y2, !,
    increment(M, X, Y1),
    NY1 is Y1 + 1,
    render_line_vert(X, NY1, Y2, M).

render_line_vert(_, _, _, _).

render_line_diag(Y, X1, X2, M, Inc) :-
    X1 =< X2, !,
    increment(M, X1, Y),
    NX1 is X1 + 1,
    NY is Y + Inc,
    render_line_diag(NY, NX1, X2, M, Inc).

render_line_diag(_, _, _, _, _).


increment(M, X, Y) :-
    nth_elem(X, Y, M, R),
    deepen(R).

deepen(A) :- \+ var(A), !, A =[B], deepen(B).
deepen([_]).

depth(A, N) :- \+ var(A), !, A =[B], depth(B, N1), N is N1 + 1. 
depth(_, 0).


is_line([[X1, Y1], [X2, Y2]]) :-
    X1 = X2 ;
    Y1 = Y2.


nth_elem(X, Y, M, R) :-
    nth0(Y, M, L),
    nth0(X, L, R).

make_matrix(_, -1, []) :- !.

make_matrix(X, Y, [L | T]) :-
    make_line(X, L),
    NY is Y - 1,
    make_matrix(X, NY, T).
    
make_line(-1, []) :- !.
make_line(X, [_| T]) :-
    NX is X - 1,
    make_line(NX, T).

get_max_x(C, R):-
    get_max_x_aux(C, R, 0).

get_max_x_aux([], R, R).

get_max_x_aux([[[X1,_],[X2,_]] | T], R, RC) :-
    RT is max(X1, X2),
    RT > RC, !,
    get_max_x_aux(T, R, RT).

get_max_x_aux([_ | T], R, RC) :-
    get_max_x_aux(T, R, RC).


get_max_y(C, R):-
    get_max_y_aux(C, R, 0).

get_max_y_aux([], R, R).

get_max_y_aux([[[_,Y1],[_,Y2]] | T], R, RC) :-
    RT is max(Y1, Y2),
    RT > RC, !,
    get_max_y_aux(T, R, RT).

get_max_y_aux([_ | T], R, RC) :-
    get_max_y_aux(T, R, RC).

/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[[[X1, Y1], [X2, Y2]]|L]) :-
    \+ at_end_of_stream(Stream),

    readWord(Stream,X1_),
    atom_number(X1_, X1),
    readWord(Stream,Y1_),
    atom_number(Y1_, Y1),
    readWord(Stream, _), % ignore ->

    readWord(Stream,X2_),
    atom_number(X2_, X2),
    readWord(Stream,Y2_),
    atom_number(Y2_, Y2),

    read_file(Stream,L).


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