%Is list

list_is_list([]).
list_is_list([_|_]).

%First element
list_first(X, [X|_]).

%Last element

list_last(X, [X]).
list_last(X, [_|T]) :- last(X, T).

%Element at position

list_element_at(X, [X|_], 1).
list_element_at(X, [_|T], K) :- K > 1, K1 is K - 1, list_element_at(X, T, K1).

%Is member

list_member(X, [X|_]).
list_member(X, [_|T]) :- list_member(X, T).

%Length

list_length([], 0).
list_length([_|T], N) :- list_length(T, N1), N is N1 + 1.

%Concatenation

list_concat([], L, L).
list_concat([X|L1], L2, [X|L3]) :- list_concat(L1, L2, L3).

% ↑
% "Concatenate" joins two specific items together.
%
% "Append" adds what you specify to whatever may already be there.
% ↓

%Append
%list_append(A, T, T) :- list_member(A, T), !.
%list_append(A, T, [A|T]).
% OR
list_append([], L, L).
list_append([H|T], X, [H|T2]) :- list_append(T, X, T2).

%Delete

%list_delete(X, [X|Xs], Xs).
%list_delete(X, [Y|Ys]) :- list_delete(X, Ys, Zs).
% OR
list_delete(X, [X], []).
list_delete(X, [X|L1], L1).
list_delete(X, [Y|L2], [Y|L1]) :- list_delete(X, L2, L1).

%Insert

list_insert(X, L, R) :- list_delete(X, R, L).

%Reverse

list_rev([], []).
list_rev([H|T], Reversed) :- list_rev(T, RevT), list_concat(RevT, [H], Reversed).

%Is permutation

list_perm([], []).
list_perm(L, [X|P]) :- list_delete(X, L, L1), list_perm(L1, P).

%Shift

list_shift([H|T], Shifted) :- list_concat(T, [H], Shifted).

%Order

list_order([X, Y|T]) :- X =< Y, list_order([Y|T]).
list_order([_]).

%Subset

list_subset([], []).
list_subset([H|T], [H|Subset]) :- list_subset(T, Subset).
list_subset([_|T], Subset) :- list_subset(T, Subset).

%Union

list_union([X|Y], Z, W) :- list_member(X, Z), list_union(Y, Z, W).
list_union([X|Y], Z, [X|W]) :- \+ list_member(X, Z), list_union(Y, Z, W).
list_union([], Z, Z).

%Intersection

list_intersect([X|Y], Z, [X|W]) :- list_member(X, Z), list_intersect(Y, Z, W).
list_intersect([X|Y], Z, W) :- \+ list_member(X, Z), list_intersect(Y, Z, W).
list_intersect([], _, []).

%Even / Odd Length

list_even_len([]).
list_even_len([_|T]) :- list_odd_len(T).

list_odd_len([_]).
list_odd_len([_|T]) :- list_even_len(T).

%Divide

list_divide([], [], []).
list_divide([X], [X], []).
list_divide([X, Y|T], [X|L1], [Y|L2]) :- list_divide(T, L1, L2).

%Min

min_(X, Y, X) :- X < Y.
min_(X, Y, Y) :- X >= Y.

list_min(X, [X]).
list_min(X, [H|T]) :- list_min(N, T), min_(H, N, X).

%Max 

max_(X, Y, X) :- X >= Y.
max_(X, Y, Y) :- X < Y.
list_max_elem([X], X).
list_max_elem([X, Y|Rest], Max) :- list_max_elem([Y|Rest], MaxRest),
max_(X, MaxRest, Max).

%Sum

list_sum([], 0).
list_sum([H|T], Sum) :- list_sum(T, SumTemp), Sum is H + SumTemp.

%Is Preffix

%list_preffix([], L).
%list_preffix([X|P], [X|L] ) :- list_prefix(P, L).
% OR
list_preffix(P, L) :- list_append(P, _, L).

%Is Suffix

list_suffix(X, L) :- list_append(_, X, L).

%Is Sublist

list_sublist(S, L) :- list_append(X, _, L), list_append(_, S, X).

%Is sorted

list_sorted([]).
list_sorted([_]).
list_sorted([X, Y|L]) :- X < Y, list_sorted([Y|L]).

%Merge sort

split([], [], []).
split([X], [X], []).
split([X, Y|R], [X|Rx], [Y|Ry]) :- split(R, Rx, Ry).

merge(X, [], X).
merge([], Y, Y).
merge([X|Rx], [Y|Ry], [X|M]) :- X =< Y, merge(Rx, [Y|Ry], M).
merge([X|Rx], [Y|Ry], [Y|M]) :- X > Y, merge([X|Rx], Ry, M).

mergesort([], []).    
mergesort([X], [X]).
mergesort([X, Y|R], S) :- split([X, Y|R], L1, L2), mergesort(L1, S1), mergesort(L2, S2), merge(S1, S2, S).