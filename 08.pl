main :-
    open('test.txt', read, Str),
    read_file(Str,Codes),
    close(Str), !,
    count(Codes, N),
    write(N), nl,
    compute_all(Codes, C),
    write(C).


compute_all([], 0).
compute_all([H | T], N) :-
    compute_all(T, N1),
    compute(H, C),
    N is N1 + C.

compute([C , K], N) :-
    simple_guesses(C, G),
    exclude(check, C, CodeG),
    n_guessed(G, O),
    complex_guesses(CodeG, G, O),
    result(G, K, N, 0).

result(_, [], N, N).

result(Guess, [H | T], N, C) :-
    nth0(I, Guess, H),
    C1 is C * 10 + I,
    result(Guess, T, N, C1).

complex_guesses(_, _, 10) :- !.

complex_guesses(Code, Guess, _) :-
    complex_guesses_aux(Code, Guess),
    n_guessed(Guess, C),
    complex_guesses(Code, Guess, C).

complex_guesses_aux([],_).

complex_guesses_aux([H | T], Guess) :-
    complex_guess_aux(Guess, H),
    complex_guesses_aux(T, Guess).

complex_guess_aux(Guess, Code) :- % check for 3
    length(Code, 5), 
    nth0(1, Guess, One), 
    subset(One, Code), !,
    nth0(3, Guess, Code).

complex_guess_aux(Guess, Code) :- 
    length(Code, 5), 
    nth0(3, Guess, T), 
    nth0(4, Guess, F),

    \+ var(T), !,
    complex_2_5(Guess, Code, T, F).

complex_guess_aux(Guess, Code) :- 
    length(Code, 6), 
    nth0(3, Guess, T), 
    nth0(4, Guess, F),
    nth0(5, Guess, Fi),
    \+ var(T),  \+ var(Fi), !,
    complex_0_6_9(Guess, Code, T, F, Fi).

complex_guess_aux(_,_).



complex_2_5(Guess, Code, T, F) :- % check for 2
    append(T, F, TF),
    sort(TF, TFS),
    remove_elements(Code, TFS, [_]), !,
    nth0(2, Guess, Code).
    
complex_2_5(Guess, Code, T, F) :- % check for 5
    append(T, F, TF),
    sort(TF, TFS),
    remove_elements(Code, TFS, []), !,
    nth0(5, Guess, Code).



complex_0_6_9(Guess, Code, T, F, _) :- % check for 9
    append(T, F, TF),
    sort(TF, TFS),
    remove_elements(Code, TFS, []), !,
    nth0(9, Guess, Code).
    
complex_0_6_9(Guess, Code, _, _, Fi) :- % check for 0
    remove_elements(Code, Fi, [_,_]), !,
    nth0(0, Guess, Code).

complex_0_6_9(Guess, Code, _, _, _) :- % check for 6
    nth0(6, Guess, Code).
    
    


    

n_guessed(Guess, N) :-
    maplist(is_var, Guess, T),
    foldl(plus, T, 0, N).



is_var(V, 1) :- \+ var(V), !.
is_var(_, 0).



simple_guesses([], [_,_,_,_,_,_,_,_,_,_]).

simple_guesses([H | T], Guess) :-
    simple_guess(H, N), !,
    simple_guesses(T, Guess),
    nth0(N, Guess, H).

simple_guesses([_ | T], Guess) :-
    simple_guesses(T, Guess).
    

simple_guess(C, 1) :- length(C, 2), !.
simple_guess(C, 7) :- length(C, 3), !.
simple_guess(C, 4) :- length(C, 4), !.
simple_guess(C, 8) :- length(C, 7), !.

count([], 0).

count([[_, []] | T], N) :-
    count(T, N).

count([[A, [C | T]] | TT], N) :-
    check(C), !,
    count([[A, T] | TT], N1),
    N is N1 + 1.

count([[A, [_ | T]] | TT], N) :-
    count([[A, T] | TT], N).


check(C) :- length(C, 2) ; length(C, 3); length(C, 4); length(C, 7).


remove_elements([], _, []) :- !.
remove_elements(R, [], R) :- !.

remove_elements([H | T1], [H | T2], R) :- !,
    remove_elements(T1, T2, R).

remove_elements([H1 | T1], [H2 | T2], [H1 | R]) :-
    H1 < H2, !,
    remove_elements(T1, [H2 | T2], R).

remove_elements(A, [_ | T2], R) :- !,
    remove_elements(A, T2, R).



/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[[C1S, C2]|L]) :-
    \+ at_end_of_stream(Stream),

    readWords(Stream,C1, 10),
    sort(C1, C1S),
    readWord(Stream, _), % ignore delimiter
    readWords(Stream, C2, 4),
    read_file(Stream,L).

/*
    
    Code addapted from
    http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse54

*/

readWords(_, [], 0).

readWords(InStream, [H | T], N) :-
    readWord(InStream, H),
    N1 is N - 1,
    readWords(InStream, T, N1).

readWord(InStream,_):-
        ignore_spaces(InStream).

readWord(InStream, W) :- !,
         get_code(InStream,Char),
         checkCharAndReadRest(Char,Chars,InStream),
         sort(Chars,W).

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