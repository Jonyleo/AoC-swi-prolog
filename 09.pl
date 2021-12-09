main :-
    open('test.txt', read, Str),
    read_file(Str,CaveT),
    close(Str), !,
    maplist(convert, CaveT, Cave),
    get_holes(Cave, Hole_Pos),
    danger_holes(Cave, Hole_Pos, R),
    write(R), nl,
    
    get_basins(Cave, Hole_Pos, Bs),
    sort(0, @>=, Bs, [B1, B2, B3 | _]),
    Res is B1 * B2 * B3,
    write(Res).





get_basins(Cave, P, Bs) :-
    maplist(helper, Cave, BFS),
    get_basins(Cave, BFS, P, Bs).

get_basins(_, _, [], []).

get_basins(Cave, BFS, [[X, Y] | T], [S | R]) :-
    get_basins(Cave, BFS, T, R),
    get_basin(X, Y, BFS, Cave, S).

helper([H | T], R) :- !, maplist(helper, [H | T], R).
helper(9,1) :- !.
helper(_, _).

danger_holes(_, [], 0).

danger_holes(Cave, [[X, Y] | P], R) :-
    nthm(X, Y, Cave, D),
    danger_holes(Cave, P, RT),
    R is D + RT + 1.


get_basin(X, Y, BFS, Cave, 0) :-
    ( length(Cave, H), Y >= H , !);
    ( nth0(0, Cave, L), length(L, W), X >= W, !);
    (X = -1, !);
    (Y = -1, !);
    nthm(X,Y,BFS, V), \+ var(V), !.

get_basin(X, Y, BFS, Cave, S) :-
    XL is X - 1,
    XR is X + 1,
    YT is Y - 1,
    YB is Y + 1,
    nthm(X,Y,BFS, 1),
    get_basin(XL, Y, BFS, Cave, SL),
    get_basin(XR, Y, BFS, Cave, SR),
    get_basin(X, YT, BFS, Cave, ST),
    get_basin(X, YB, BFS, Cave, SB), 
    S is SL + SR + ST + SB + 1.   

get_holes(Cave, P) :-
    length(Cave, H),
    nth0(0, Cave, L),
    length(L, W),
    get_holes(Cave, P, W, H, W).

get_holes(_, [], _, -1, _) :- !.

get_holes(Cave, P, -1, Y, W) :- !,
    Y1 is Y - 1,
    get_holes(Cave, P, W, Y1, W).

get_holes(Cave, [[X, Y] | P], X, Y, W) :-
    is_hole(X, Y, Cave), !,
    X1 is X - 1,
    get_holes(Cave, P, X1, Y, W).

get_holes(Cave, P, X, Y, W) :-
    X1 is X - 1,
    get_holes(Cave, P, X1, Y, W).


is_hole(X, Y, M) :- !,
    is_left(X, Y, M),
    is_right(X, Y, M),
    is_top(X, Y, M),
    is_bot(X, Y, M).

is_left(0, _, _) :- !.
is_left(X, Y, M) :- 
    nthm(X, Y, M, E),
    XL is X - 1,
    nthm(XL, Y, M, EL),
    E < EL.

is_right(X, _, M) :- 
    nth0(0, M, L),
    length(L, S),
    X >= S - 1, !.

is_right(X, Y, M) :- 
    nthm(X, Y, M, E),
    XR is X + 1,
    nthm(XR, Y, M, EL),
    E < EL.


is_top(_, 0, _) :- !.
is_top(X, Y, M) :- 
    nthm(X, Y, M, E),
    YT is Y - 1,
    nthm(X, YT, M, EL),
    E < EL.

is_bot(_, Y, M) :- 
    length(M, S),
    Y >= S - 1, !.

is_bot(X, Y, M) :- 
    nthm(X, Y, M, E),
    YB is Y + 1,
    nthm(X, YB, M, EL),
    E < EL.


convert([H|T], R) :- !, maplist(convert,[H|T], R).
convert(N, R) :- R is N - 48.

nthm(X, Y, M, E) :-
    nth0(Y, M, L),
    nth0(X, L, E).


/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),

    readWord(Stream,X),
    read_file(Stream,L).

/*
    
    Code addapted from
    http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse54

*/

readWord(InStream,_):-
        ignore_spaces(InStream).

readWord(InStream, W) :- !,
         get_code(InStream,Char),
         checkCharAndReadRest(Char,W,InStream).


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