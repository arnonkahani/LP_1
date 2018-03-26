% utils
check_p_for_all(P,N,X,[Y|Ys]) :-
   call(P, N,X, Y),
   check_p_for_all(P,N, X, Ys).

check_p_for_all(_,_,_ ,[]).

is_DNA_charchter('C').
is_DNA_charchter('A').
is_DNA_charchter('T').
is_DNA_charchter('G').

dna_length(Xs,L) :- dna_length(Xs,0,L).

dna_length([],L,L).

dna_length([_|Xs],T,L) :-
    T1 is T+1,
    dna_length(Xs,T1,L).

all_dna([]).
all_dna([H|T]) :- is_DNA_charchter(H),all_dna(T). 

is_dna_length(N,L) :-
    all_dna(L),
    dna_length(L,M),
    M == N.
% Task 1

% set rule for counting C and G.

is_C_or_G('C').
is_C_or_G('G').
    
% use counter to find how many instance of C and G.

percentage(N, Word) :-
    is_dna_length(N,Word),
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

hamming(N,[], [],C) :-
    F is floor(N/2),
    C >= F.

hamming(N,[A|As],[B|Bs],C) :-
    is_DNA_charchter(A),
    is_DNA_charchter(B),
    A \== B,
    C1 is C + 1,
    hamming(N,As,Bs,C1).

hamming(N,[A|T1],[B|T2],C) :-
    A == B,
    hamming(N,T1,T2,C).

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


is_dna(N, M, Words) :-
    is_dna(N, M, Words,0).

is_dna(_, M,[],C) :-
    C == M.

is_dna(N, M, [A|As],C) :-
    percentage(N,A),
    check_p_for_all(hamming,N,A,As),
    check_p_for_all(reverse_complement,N,A,As),
    Cn is C + 1,
    is_dna(N, M, As,Cn).

% Task 8

dna_word(N, [H|T]):-
    dna_letter(H),
    Ns is N - 1,
    Ns>=0,
    dna_word(Ns,T).

dna_word_list(_,0,[]).

dna_word_list(N,M, [H|T]):-
    dna_word(N,H),
    Ms is M - 1,
    Ms>=0,
    dna_word_list(N,Ms,T).

dna(N, M, Words) :-
    dna_word_list(N,M,Words),
    is_dna(N,M,Words).




