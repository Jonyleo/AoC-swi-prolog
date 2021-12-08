main :-
    open('test.txt', read, Str),
    read_file(Str,Fish),
    close(Str), !,

    gen_count(Fish, Fish_Ns, 8),
    simulate_days(Fish_Ns, Fish_Ns1,256),
    foldl(plus, Fish_Ns1, 0, R),

    write(R).

simulate_days(Fish, Fish, 0).

simulate_days(Fish, Fish_T, N) :-
    simulate_day(Fish, Fish_R),
    N1 is N - 1,
    simulate_days(Fish_R, Fish_T, N1).


simulate_day(Fish, [N0,N1,N2,N3,N4,N5,N6,N7,N8]) :-
    [Breed | Rest] = Fish,
    append(Rest, [Breed], Fish_T),
    nth0(6, Fish_T, T),
    [N0,N1,N2,N3,N4,N5,T,N7,N8] = Fish_T,
    N6 is T + Breed.



gen_count(Crabs, Crabs_N, S) :-
    gen_count_aux(Crabs, Crabs_N, S, 0).

gen_count_aux(_, [], S, C) :- C > S, !.

gen_count_aux(Crabs, [H | T], S, N) :-
    include(=(N), Crabs, Equal),
    length(Equal, H),
    N1 is N + 1,
    gen_count_aux(Crabs, T, S, N1).

increment(L, X) :-
    nth0(X, L, R),
    deepen(R).

deepen(A) :- \+ var(A), !, A =[B], deepen(B).
deepen([_]).

depth(A, N) :- \+ var(A), !, A =[B], depth(B, N1), N is N1 + 1. 
depth(_, 0).


/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),

    readWord(Stream,X_),
    atom_number(X_, X),

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