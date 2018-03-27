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

hamming(N,[X|T1],[X|T2],C) :-
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

is_dna_process_2(_,_,[]).

is_dna_process_2(N,C,[H|T]) :-
    hamming(N,C,H),
    reverse_complement(N,C,H),
    is_dna_process_2(N,C,T).

is_dna_process_1(N, M, [H|T]) :-
    percentage(N,H),
    is_dna_process_2(N,H,T),
    Mn is M - 1,
    is_dna(N, Mn,T).

is_dna(_,0,_).

is_dna(N, M, Words) :-
    is_dna_process_1(N, M, Words).


% Task 5


dna_letter('A').

dna_letter('C').

dna_letter('G').

dna_letter('T').

dna_word(0,[]).

dna_word(N, [H|T]):-
    dna_letter(H),
    Ns is N - 1,
    Ns>=0,
    dna_word(Ns,T).

% Task 6

num_letter(0,'A').

num_letter(1,'C').

num_letter(2,'G').

num_letter(3,'T').

num_to_word([],[]).


num_to_word([Hn|Tn], [Hl|Tl]):-
    num_letter(Hn,Hl),
    num_to_word(Tn,Tl).

zero_list(0,[]).

zero_list(N,[0|Rest]):-
    N>0,
    Ns is N -1,
    zero_list(Ns, Rest).


dec_to_quat(0,[]).

dec_to_quat(Rn,List):-
    Num is mod(Rn,4),
    Rn>0,
    Re is div(Rn,4),
    dec_to_quat(Re,List2),
    append(List2,[Num],List).

dec_to_quat(N,R,List):-
    dec_to_quat(R,Word),
    length(Word,Wl),
    Zero_num is N-Wl,
    Zero_num == 0,
    dec_to_quat(R, List).

dec_to_quat(N,R,List):-
    dec_to_quat(R,Word),
    length(Word,Wl),
    Zero_num is N-Wl,
    Zero_num > 0,
    zero_list(Zero_num, Zl),
    append(Zl,Word,List).

list_from_num(N,Rn,Word):-
    dec_to_quat(N,Rn,Num_List),
    num_to_word(Num_List,Word).

delMember(_, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).

select_elem(0,[H|_],H).

select_elem(N,[_|T],H):-
    N>0,
    Ns is N-1,
    select_elem(Ns,T,H).

mix_list(N,[N],[N]).

mix_list(N,L1,[N|Mix_L1]):-
    delMember(N,L1,L2),
    length(L2,Length),
    Length>0,
    Len is Length - 1,
    random_between(0,Len,R),
    select_elem(R,L2,Re),
    mix_list(Re,L2,Mix_L1).


make_random_list(N,R,List):-
    numlist(0,N,N_list),
    mix_list(R,N_list,List).

random_dna_word(N,Word):-
   pow(4,N,Ns),
   Nss is Ns-1,
   random_between(0,Nss,R),
   make_random_list(Ns,R,List),
   random_dna_word(N,Word,List).

random_dna_word(N,Word,List):-
   select(Ra,List,_),
   list_from_num(N,Ra,Word).

% Task 7

% Task 8


dna_word_list(_,0,[]).

dna_word_list(N,M, [H|T]):-
    dna_word(N,H),
    Ms is M - 1,
    Ms>=0,
    dna_word_list(N,Ms,T).

dna(N, M, Words) :-
    dna_word_list(N,M,Words),
    is_dna(N,M,Words).


% Task 9

% Task 10

percentage_subset(_,[],[]).

percentage_subset(N,[H|T1],[H|T2]):-
    percentage(N,H),
    percentage_subset(N,T1,T2).

percentage_subset(N,[_|T1],T2):-
    percentage_subset(N,T1,T2).


dna_subset(_, [], []).

dna_subset(N, Words, Subset):-
    percentage_subset(N,Words,Subset),
    length(Subset,Len),
    is_dna(N,Len,Subset).




















