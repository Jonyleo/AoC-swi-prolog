main :-
    open('test.txt', read, Str),
    read_file(Str,Lines),
    close(Str), !,
    increases_single(Lines, Total1),
    increases_triple(Lines, Total2),
    write(Total1), nl,
    write(Total2), nl.
    

increases_single([_], 0).

increases_single([P, C | T], Res) :-
	C > P, !,
	increases_single([C | T], Res_mid),
	Res is Res_mid + 1.

increases_single([_, C | T], Res) :-
	increases_single([C | T], Res).


increases_triple([_,_,_], 0).

increases_triple([A, B, C, D | T], Res) :-
    B+C+D > A+B+C, !,
    increases_triple([B, C, D | T], Res_mid),
    Res is Res_mid + 1.

increases_triple([_, B, C, D | T], Res) :-
    increases_triple([B, C, D | T], Res).


/*
    
    Code addapted from
    https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog

*/
read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    readWord(Stream,SX),
    atom_number(SX, X),
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