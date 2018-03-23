% utils

check_p_for_all(_,_,_ ,[]).
check_p_for_all(P,N,X,[Y|Ys]) :-
   call(P, N,X, Y),
   check_p_for_all(P,N, X, Ys).

% Task 1

% set rule for counting C and G.

is_C_or_G('C').
is_C_or_G('G').
    
% use counter to find how many instance of C and G.

percentage(N, Word) :- 
    percentage(N, 0, Word). 

% list is empty or end case.

percentage(N, A, []) :- 
    C is ceiling(N/2),
    F is floor(N/2),
    A >= F,
    A =< C.

% is a C or G charcter.

percentage(N, C, [H|T]) :-
    is_C_or_G(H),
    C1 is C + 1,
    percentage(N, C1, T).

% is not a C or G charcter.

percentage(N, C, [H|T]) :-
    \+ is_C_or_G(H),
    percentage(N, C, T).

% Task 2


hamming(N, Word1, Word2) :-
    hamming(N,Word1,Word2,0).

hamming(N,[A|As],[B|Bs],C) :-
    A \== B,
    C1 is C + 1,
    hamming(N,As,Bs,C1).

hamming(N,[_|T1],[_|T2],C) :-
    hamming(N,T1,T2,C).

hamming(N,[], [],C) :-
    F is floor(N/2),
    C >= F.

% Task 3
reverse_complement(N, Word1, Word2) :-
    reverse_w(Word1,Wr,[]),
    switch_w(Word2,Ws,[]),
    hamming(N,Wr,Ws).

switch_w([H|T],Ws,Acc) :-
    H == 'C',
    switch_w(T,Ts,Acc),
    append(['G'],Ts,Ws).
switch_w([H|T],Ws,Acc) :-
    H == 'A',
    switch_w(T,Ts,Acc),
    append(['T'],Ts,Ws).
switch_w([H|T],Ws,Acc) :-
    H == 'T',
    switch_w(T,Ts,Acc),
    append(['A'],Ts,Ws).
switch_w([H|T],Ws,Acc) :-
    H == 'G',
    switch_w(T,Ts,Acc),
    append(['C'],Ts,Ws).


switch_w([],Ws,Ws).


reverse_w([],Wr,Wr).

reverse_w([H|T],Wr,Acc) :- 
    reverse_w(T,Wr,[H|Acc]).

% Task 4


is_dna(_,_,[],[],_). 

is_dna(N, M, [A|_],[_|Bs]) :-
    percentage(N,A),
    check_p_for_all(hamming,N,A,Bs),
    check_p_for_all(reverse_complement,N,A,Bs),
    is_dna(N, M, Bs,Bs,0).


is_dna(N, _, [A|_],[_|_]) :-
    \+ percentage(N,A),
    is_dna(N, 0, [],[],1).

is_dna(N, _, [A|_],[_|Bs]) :-
    \+ check_p_for_all(hamming,N,A,Bs),
    is_dna(N, 0, [],[],1).

is_dna(N, _, [A|_],[_|Bs]) :-
    \+ check_p_for_all(reverse_complement,N,A,Bs),
    is_dna(N, 0, [],[],1).

is_dna(N, M, Words) :-
    is_dna(N, M, Words,Words,R),
    R == 1.


