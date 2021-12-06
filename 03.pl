main :-
    open('test.txt', read, Str),
    read_file(Str,Lines),
    close(Str), !,
    calculate_gamma(Lines, G),
    calculate_epsilon(Lines, E),
    Res is E * G,
    write(Res), nl,
    calculate_oxigen(Lines, O),
    calculate_scrubber(Lines, S),
    Res1 is O * S,
    write(Res1).


gamma_func([H | T], B) :- nth0(S, H, _), common_bit([H | T], B, S, 0).

calculate_gamma(Lines, G) :-
	findall(B, gamma_func(Lines, B), G_array),
	convert_binary(G_array, G).



epsilon_func([H | T], B) :- nth0(S, H, _), common_bit([H | T], Bt, S, 0), B is (Bt-1) * (-1).

calculate_epsilon(Lines, E) :-
	findall(B, epsilon_func(Lines, B), G_array),
	convert_binary(G_array, E).



calculate_oxigen(Lines, R) :-
	calculate_oxigen_aux(Lines, R, 0).


calculate_oxigen_aux([Res], R, _) :-
	convert_binary(Res, R).

calculate_oxigen_aux(Lines, R, N) :-
	common_bit(Lines, B, N, 0),
	include(has_bit_N(B, N), Lines, Remainder),
	N_next is N+1,
	calculate_oxigen_aux(Remainder, R, N_next).



calculate_scrubber(Lines, R) :-
	calculate_scrubber_aux(Lines, R, 0).

calculate_scrubber_aux([Res], R, _) :-
	convert_binary(Res, R).

calculate_scrubber_aux(Lines, R, N) :-
	common_bit(Lines, Bt, N, 0),
	B is (Bt - 1) * (-1), 
	include(has_bit_N(B, N), Lines, Remainder),
	N_next is N+1,
	calculate_scrubber_aux(Remainder, R, N_next).



has_bit_N(B, N, Bits) :- nth0(N, Bits, B).


common_bit([], 0, _, R) :- R > 0 , !.
common_bit([], 1, _, _) :- !.

common_bit([H | T], B, N, R) :-
	nth0(N, H, 0), !,
	R1 is R + 1,
	common_bit(T, B, N, R1).

common_bit([H | T], B, N, R) :- 
	nth0(N, H, 1), !,
	R1 is R - 1, 
	common_bit(T, B, N, R1).


convert_binary(Bits, R) :-
	convert_binary_aux(Bits, R, 0).

convert_binary_aux([], R, R).

convert_binary_aux([H|T], R, C) :- 
	Rt is C * 2 + H,
	convert_binary_aux(T, R, Rt).

/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[W|L]) :-
    \+ at_end_of_stream(Stream),
    readWord(Stream,TW),
    convert_word(TW, W),
    read_file(Stream,L).

convert_word([],[]).

convert_word([H | T], [H1 | T1]) :- 
	H1 is H - 48,
	convert_word(T, T1).



/*
    
    Code addapted from
    http://www.let.rug.nl/bos/lpn//lpnpage.php?pagetype=html&pageid=lpn-htmlse54

*/

readWord(InStream,W):-
         get_code(InStream,Char),
         checkCharAndReadRest(Char,W,InStream).
   
   
checkCharAndReadRest(10,[],_):-  !.

checkCharAndReadRest(32,[],_):-  !.

checkCharAndReadRest(-1,[],_):-  !.

checkCharAndReadRest(end_of_file,[],_):-  !.

checkCharAndReadRest(Char,[Char|Chars],InStream):-
     get_code(InStream,NextChar),
     checkCharAndReadRest(NextChar,Chars,InStream). 