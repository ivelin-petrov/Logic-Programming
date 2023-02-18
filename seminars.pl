:- use module(library(clpfd)).
% ; - more results

nat(c).
nat(s(X)) :- nat(X).

naturals(0).
naturals(X) :- naturals(Y), X is Y + 1.

is_not_prime(X) :- X1 is X - 1, bet2(2, X1), X mod Y =:= 0.
all_primes(X) :- naturals(X), X > 1, not(is_not_prime(X)).

fibonacci_cons(0, 0, 1).
fibonacci_cons(N, A, B) :- N > 0, N1 is N - 1, fibonacci_cons(N1, X, A), B = A + X.
fibonacci_interval(N, M, A, B) :- bet2(0, N, M), fibonacci_cons(M, A, B). 
all_fibonacci(F) :- naturals(N), fibonacci_cons(N, F, _).

fibonacci_bound(N, 0, 1) :- 0 #<= N.
fibonacci_bound(N, A, B) :- A #> 0, X #>=0, A #<= N, B #= X + A, fibonacci_bound(N, X, A).

fib_rec(0, 1).
fib_rec(A, 0) :- A > 0, B >= A, C is B - A, fib_rec(C, A).
all_fibonacci(F) :- naturals(F), bet2(0, F, A), fib_rec(A, F).

sliced_pairs(N, A, B) :- bet2(0, N, A), B is N - A.
pairs(A, B) :- naturals(N), sliced_pairs(N, A, B).
%генератора на B не връща контрола на A и се трупат стойности само при B
pairs2(A, B) :- naturals(A), naturals(B).

gen_list(0, 0, []).
gen_list(L, S, [H|T]) :- L > 0, bet2(0, S, H), S1 is S - H, L1 is L - 1, gen_list(L1, S1, T).
gen_all_lists(List) :- pairs(L, S), gen_list(L, S, List).

%семантично същата програма, но има риск за out of local stack
%nat(s(X)) :- nat(X).
%nat(c).

sum(X, c, X).
sum(X, s(Y), s(Z)) :- sum(X, Y, Z).

%app(X, Y, [1, 2, 3, 4, 5]).
append_([], L2, L2).
append_([H|T], L2, [H|L]) :- append_(T, L2, L).

member2(X, L) :- member2(Z1, [X|Z2], L).
%member_(X, L) :- member_(_, [X|_], L).

%unification t1=t2
%test equality t1=:=t2
%set value: X is t1, t1 does not contain variables
%=<, >=, =\=, =:=
%restrictions: T1 #= T2, requires T1 to become equal to T2, i.e. search evaluations in N s.t. 
%T1=:=T2, T1 #< T2, T1 #> T2

%length_([], 0).
%length_([_|T], N) :- N is N1 + 1, length_(T, N1).   NOT WORKING !!!!!

length_([], 0).
length_([_|T], N) :- length_(T, N1), N is N1 + 1.

%len2([], N) :- N #= 0.
%len2([H|T], N) :- N #= N1 + 1, len2(T, N1).

%How to shuffle
% 1. permute (T, PT)
% 2. insert H at random position in PT
%   2.1 PT = PT1, PT2(app)
%   2.2 P = P1, [H], PT2(app) 

permute([], []).
permute([H|T], P) :- permute(T, PT), append_(PT1, PT2, PT), append_(PT1, [H|PT2], P).

incr([]).
%incr([X]).
incr([_]).
incr([X, Y|T]) :- X =< Y, incr([Y|T]).
sort2(L, S) :- permute(L, S), incr(L).

%not_incr(S) :- append_(L1, [X, Y|L2], S), X > Y.
not_incr(S) :- append_(_, [X, Y|_], S), X > Y.
incrl(S) :- not(not_incr(S)).

%merge sort
%1. split(L, L1, L2) with [L1|appr. equal to|L2].
%2. sort(L1, S1), sort(L2, S2).
%3. merge(S1, S2, S) merge the sorted S1 and S2 into S.

%1. split(L, Even, Odd) - positions
split([], [], []).
split([X], [], [X]).
split([X, Y|L], [Y|Even], [X|Odd]) :- split(L, Even, Odd).
merge2([], S2, S2).
merge2(S1, [], S1).
merge2([H1|S1], [H2|S2], [H1|S]) :- H1 =< H2, merge2(S1, [H2|S2], S).
merge2([H1|S1], [H2|S2], [H2|S]) :- H2 < H1, merge2([H1|S1], S2, S).
merge_sort([], []).
merge_sort([X], [X]).
merge_sort([X, Y|L], S) :- split([X, Y|L], Even, Odd), merge_sort(Even, Evens), merge_sort(Odd, Odds),
                           merge2(Evens, Odds, S).

%subset(L, S) - generate in S all possible subsequences of L
subset([], []).
subset([H|T], [H|ST]) :- subset(T, ST).
subset([_|T], ST) :- subset(T, ST).

%--------------------------------------------------Graphs------------------------------------------------------

%Generate all cliques in G-graph. Generate only the maximal ones.
%U is clique <=> \forall x, y\in U (xy\in E)
%U is not clique <=> \exists x,y\in U (x \neq y & xy\not\in E)
%xy \in E <=> [x|y] is a member of E or [y|x] is a member of E.
edge(X, Y, E) :- member_([X|Y], E).
edge(X, Y, E) :- member_([Y|X], E).

is_not_clique(U, E) :- member_(X, U), member_(Y, U), not(X = Y), not(X, Y, E).

gen_clique(V, E, C) :- subset(V, C), not(is_not_clique(C, E)).

is_not_subset(U, U1) :- member_(X, U), not(member_(X, U1)).
not_equal_sets(U, U1) :- is_not_subset(U, U1).
not_equal_sets(U, U1) :- is_not_subset(U1, U).

cond_not_inclw_max_clique(E, U, U1) :- not_equal_sets(U, U1), not(is_not_subset(U, U1)), not(is_not_clique(U1, E)).
not_max_inclw_clique(V, E, U) :- subset(V, U1), cond_not_inclw_max_clique(E, U, U1).
max_inclw_clique(V, E, U) :- gen_clique(V, E, U), not(not_max_inclw_clique(V, E, U)).

%Hamiltonian cycle
%last_first_edge(P, E) :- append_([F], T, P), append_(H, [L], P), edge(F, L, E).
last_first_edge(P, E) :- append_([_], _, P), append_(_, [_], P), edge(_, _, E).
not_path(P, E) :- append_(_, [U, V|_], P), not(edge(U, V, E)).
hamiltonian_cycle(V, E, C) :- permute(V, C), not(not_path(C, E)), last_first_edge(C, E).

%Maximal size clique
%Coloring of graph, given G and colors C, determine whether and how the vertices of G can be
%colored in C colors, i.e. for each edge (uv) in G u and v are not monochromatic

gcd(A, 0, A).
gcd(A, B, D) :- B > 0, A >= B, A1 is A - B, gcd(A1, B, D).
gcd(A, B, D) :- A < B, gcd(B, A, D).

gcd_list([], 0).
gcd_list([H|T], D) :- gcd_list(T, D1), gcd(H, D1, D).

coprime_list(L, P) :- subset2(L, P), gcd_list(P, D), D =:= 1.
not_minimal_coprime_list(L, P) :- coprime_list(L, P1), len2(P1, N1), N1 < N.
minimal_coprime_list(L, P) :- coprime_list(L, P), not(not_minimal_coprime_list(L, P)).

bet2(X, Y, X) :- X =< Y.
bet2(X, Y, Z) :- X < Y, X1 is X + 1, bet2(X1, Y, Z).
%bet2(X, Y, Z) :- bet2(X, Y, Z1), Z1 < Y, Z is Z + 1.

divisor(A, B, D) :- A div D =:= 0, B div D =:= 0.
common_divisors(A, B, D) :- bet2(1, A, D), divisor(A, B, D).

not_greatest_common_div(A, B, D) :- common_divisors(A, B, D1), D < D1.
greatest_common_div(A, B, D) :- common_divisors(A, B, D), not(not_greatest_common_div(A, B, D)).

not_common_divisor_of_list(L, D) :- mem2(A, L), A mod D =\= 0.
common_divisor_of_list([H|T], D) :- bet2(1, H, D), not(not_common_divisor_of_list([H|T], D)).
not_greatest_divisor_of_list([H|T], D) :- common_divisor_of_list([H|T], D1), D < D1.
greatest_divisor_of_list([H|T], D) :- common_divisor_of_list([H|T], D), not(not_greatest_divisor_of_list([H|T], D)).

map_list_to_list_of_pairs([], []).
map_list_to_list_of_pairs([A, T], [[L, S], T1]) :- bet2(B, A, L), S is A - L, map_list_to_list_of_pairs(T, T1).

map_list_of_pairs_to_llists([], []).
map_list_of_pairs_to_llists([[L, S] | T], [L1|T1]) :- gen_list(L, S, L1), map_list_of_pairs_to_llists(T, T1).

map_list_to_llist(L, G) :- map_list_to_list_of_pairs(L, P), map_list_of_pairs_to_llists(P, G).

gen_llist(G) :- naturals(N), bet2(0, N, L), S is N - L, gen_list(L, S, Lnat), map_list_to_llist(Lnat, G).

not_all_odd(G) :- member2(L, G), length_(L, len), len mod 2 =\= 1.
all_odd(G) :- not(not_all_odd(G)).

not_all_lists_empty(G) :- member2([], G).
not_all_lists_sets(G) :- member2(L, G), append_(_, [X|S], L), member2(X, S).
all_lists_nonempty_sets(G) :- not(not_all_lists_empty(G)), not(not_all_lists_sets(G)).

not_unique_lists(G) :- append_(_, [V|T]|G1, G), member2([V|T1], G1).
unique_lists :- not(not_unique_lists(G)).

missing_vertex(G) :- member2([V|T], G), member2(U, T), not(member2([U, _), G])).
no_missing_vertex(G) :- not(missing_vertex(G)).

not_symmetric(G) :- member2([V|T], G), member2(U, T), member2([U|T1], G), (not(member2(V, T1))).
symmetric2(G) :- not(not_symmetric(G)).

is_graph(G) :- all_lists_nonempty_sets(G), unique_lists(G), no_missing_vertex(G), symmetric2(G).

reachable(G, V, V, 0).
reachable(G, V, U, N) :- N > 0, member2([V|Nv], G), member2(W, Nv), N1 is N - 1, reachable(G, W, U, N1).
not_reachable(G, V, U, MaxN) :- between_(0, MaxN, N), reachable(G, V, U, N).

not_connected([[V|Nv]|G1]) :- len(G1, MaxN), member2([U|_], G1), not(not_reachable([[V|Nv]|G1], V, U, MaxN)).
connected(G) :- not(not_connected(G)). 

is_Eulerian(G) :- is_graph(G), connected(G), all_odd(G).
generate_Eulerian(G) :- gen_llist(G), is_Eulerian(G), len(G, N).