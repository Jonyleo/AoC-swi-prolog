main :-
    open('test.txt', read, Str),
    read_file(Str,Cs),
    close(Str), !,
    get_corrupts(Cs, R),
    calc_score_corrupt(R, S),
    write(S), nl,
    calc_score_incomplete(Cs, Ss),
    sort(0, @>=, Ss, SSs),
    length(SSs, L),
    L1 is (L - 1)/2,
    nth0(L1, SSs, Score),
    write(Score).


calc_score_incomplete([], []).
calc_score_incomplete([H | T], [S | R]) :-
    is_incomplete(H, I), !,
    calc_score_incomplete_aux(I, S),
    calc_score_incomplete(T, R).


calc_score_incomplete([_ | T], R) :-
    calc_score_incomplete(T, R).

calc_score_incomplete_aux(L, N) :-
    calc_score_incomplete_aux(L, N, 0).

calc_score_incomplete_aux([], N, N).

calc_score_incomplete_aux([H | T], N, C) :-
    score_incomplete(H, P),
    C1 is C * 5 + P,
    calc_score_incomplete_aux(T, N, C1).


calc_score_corrupt([], 0).

calc_score_corrupt([H | T], R) :-
    calc_score_corrupt(T, Rt),
    score_corrupt(H, P),
    R is Rt + P.

get_corrupts([], []).

get_corrupts([H | T], [U | R]) :-
    is_corrupt(H, U), !,
    get_corrupts(T, R).

get_corrupts([_ | T], R) :-
    get_corrupts(T, R).

is_corrupt(L, U) :-
    check_code(L, [], U),
    \+ is_list(U).

is_incomplete(L, U) :-
    check_code(L, [], U),
    is_list(U).


check_code([], U, U).

check_code([O | T], R, U) :- 
    pair(O, C), !,
    check_code(T, [C | R], U).

check_code([C | _], [H | _], C) :-
    closer(C),
    \+ H = C, !.

check_code([C | T], [_ | RT], U) :-
    closer(C),
    check_code(T, RT, U).


score_corrupt(']', 57).
score_corrupt('>', 25137).
score_corrupt('}', 1197).
score_corrupt(')', 3).

score_incomplete(']', 2).
score_incomplete('>', 4).
score_incomplete('}', 3).
score_incomplete(')', 1).

pair('[', ']').
pair('<', '>').
pair('{', '}').
pair('(', ')').

closer(']').
closer('>').
closer('}').
closer(')').

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

checkCharAndReadRest(Char,[C|Chars],InStream):-
     atom_codes(C, [Char]),
     get_code(InStream,NextChar),
     checkCharAndReadRest(NextChar,Chars,InStream). 

ignore_spaces(InStream) :-
    peek_code(InStream, 32),
    get_code(InStream,_),
    ignore_spaces(InStream).