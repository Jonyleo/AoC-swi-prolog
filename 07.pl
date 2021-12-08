main :-
    open('test.txt', read, Str),
    read_file(Str,Crabs),
    close(Str), !,
    max_list(Crabs, S),
    gen_count(Crabs, Crabs_N, S),

    numlist(0, S, Indexes),
    zip(Indexes, Crabs_N, CrabsZ),
    sort(2, @>=, CrabsZ, Sorted), !,
    compute_costs(Sorted, Costs, linear_cost),
    sort(2, @=<, Costs, CostsS),
    [[P, C]|_] = CostsS,

    write(P), nl, write(C), nl,

    compute_costs(Sorted, CostsG, growing_cost),
    sort(2, @=<, CostsG, CostsSG),
    [[Pg, Cg]|_] = CostsSG,

    write(Pg), nl, write(Cg).

linear_cost(N, I, R) :- R is abs(N-I).
growing_cost(N, I, R) :- R is abs(N-I) * (abs(N-I) + 1) / 2.

compute_costs(Crabs, Costs, Cost_Gen) :-
    compute_costs_aux(Crabs, Crabs, Costs, Cost_Gen).

compute_costs_aux(_, [], [], _).
compute_costs_aux(Crabs, [[I, _] | T], [[I, C] | T1], Cost_Gen) :-
    compute_cost(Crabs, I, C, Cost_Gen),
    compute_costs_aux(Crabs, T, T1, Cost_Gen).

compute_cost([], _, 0, _).

compute_cost([[I, Oc] | T], N, Cost, Cost_Gen) :-
    compute_cost(T, N, C1, Cost_Gen),
    call(Cost_Gen, N, I, R),
    C is R * Oc,
    Cost is C + C1.


not_zero(Crab) :- nth0(1, Crab, S), S \= 0.

zip([], [], []).

zip([H | T], [H1 | T1], [[H, H1] | R]) :-
    zip(T, T1, R).

gen_count(Crabs, Crabs_N, S) :-
    gen_count_aux(Crabs, Crabs_N, S, 0).

gen_count_aux(_, [], S, C) :- C > S, !.

gen_count_aux(Crabs, [H | T], S, N) :-
    include(=(N), Crabs, Equal),
    length(Equal, H),
    N1 is N + 1,
    gen_count_aux(Crabs, T, S, N1).


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