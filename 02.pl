main :-
    open('test.txt', read, Str),
    read_file(Str,Lines),
    close(Str), !,
    calculate(Lines, H, V),
    calculate_aim(Lines, H1, V1),
    Res is H*V,
    Res1 is H1*V1,
    write(Res), nl,
    write(Res1), nl.


calculate([], 0, 0).

calculate([[forward, A] | R], H, V) :-
    calculate(R, H1, V),
    H is H1 + A.
    
calculate([[down, A] | R], H, V) :-
    calculate(R, H, V1),
    V is V1 + A.

calculate([[up, A] | R], H, V) :-
    calculate(R, H, V1),
    V is V1 - A.


calculate_aim(Dirs, H, V) :-
    calculate_aim_aux(Dirs, H, V, 0, 0, 0).

calculate_aim_aux([], H, V, _, H, V).

calculate_aim_aux([[forward, A] | R], F_H, F_V, Aim, H, V) :-
    N_H is H + A,
    N_V is V + (A*Aim),
    calculate_aim_aux(R, F_H, F_V, Aim, N_H, N_V).
    
calculate_aim_aux([[down, A] | R], F_H, F_V, Aim, H, V) :-
    N_A is Aim + A, 
    calculate_aim_aux(R, F_H, F_V, N_A, H, V).

calculate_aim_aux([[up, A] | R], F_H, F_V, Aim, H, V) :-
    N_A is Aim - A, 
    calculate_aim_aux(R, F_H, F_V, N_A, H, V).

/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[[Dir, Amt]|L]) :-
    \+ at_end_of_stream(Stream),
    readWord(Stream,Dir),
    readWord(Stream,Amt_mid),
    atom_number(Amt_mid, Amt),
    read_file(Stream,L).

/*
    
    Code addapted from
    http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse54

*/

readWord(InStream,W):-
         get_code(InStream,Char),
         checkCharAndReadRest(Char,Chars,InStream),
         atom_codes(W,Chars).
   
   
checkCharAndReadRest(10,[],_):-  !.

checkCharAndReadRest(32,[],_):-  !.

checkCharAndReadRest(-1,[],_):-  !.

checkCharAndReadRest(end_of_file,[],_):-  !.

checkCharAndReadRest(Char,[Char|Chars],InStream):-
     get_code(InStream,NextChar),
     checkCharAndReadRest(NextChar,Chars,InStream). 