app([],L1,L1).
app([H|L1],L2,[H|L]):-app(L1,L2,L).

mem2(X,L):-app(L1,[X|L2],L).

permute2([],[]).
permute2([H|T],P):-permute2(T,PT),app(PT1,PT2,PT),app(PT1,[H|PT2],P).

subset2([],[]).
subset2([H|T],[H|ST]):-subset2(T,ST).
subset2([H|T],ST):-subset2(T,ST).

edge(X,Y,E):-mem2([X|Y],E).
edge(X,Y,E):-mem2([Y|X],E).
is_not_clique(U,E):-mem2(X,U),mem2(Y,U),not(X=Y),not(edge(X,Y,E)).
my_graph(V,E):-V=[1,2,3,4,5,6,7],E=[[1|2],[1|3],[1|4],[1|5],[2|3],[2|4],[2|5],
                                    [3|4],[3|7],[5|6],[6|7]].
gen_clique(V,E,C):-subset2(V,C),not(is_not_clique(C,E)).

is_not_subset(U,U1):-mem2(X,U),(not(mem2(X,U1))).
not_equal_sets(U,U1):-is_not_subset(U,U1).
not_equal_sets(U,U1):-is_not_subset(U1,U).

cond_not_inclw_max_clique(E,U,U1):-not_equal_sets(U,U1),not(is_not_subset(U,U1)),
    not(is_not_clique(U1,E)).
not_inclw_max_clique(V,E,U):-subset2(V,U1),cond_not_inclw_max_clique(E,U,U1).
inclw_max_clique(V,E,U):-gen_clique(V,E,U),not(not_inclw_max_clique(V,E,U)).

% my_graph(V,E),not_inclw_max_clique(V,E,[1,2,3,4]). - false
% my_graph(V,E),inclw_max_clique(V,E,[1,2,3,5]). - false
% my_graph(V,E),inclw_max_clique(V,E,U).

last_first_edge(P,E):-app([F],T,P),app(H,[L],P),edge(F,L,E).
not_path(P,E):-app(_,[U,V|_],P),not(edge(U,V,E)).
ham_cycle(V,E,C):-permute2(V,C),not(not_path(C,E)),last_first_edge(C,E).

% my_graph(V,E),ham_cycle(V,E,C).
% ham_cycle([1,2,3],[[1|2],[2|3],[3|1]],C).

gcd(A,0,A).
gcd(A,B,D):-B>0,A>=B,A1 is A-B,gcd(A1,B,D).
gcd(A,B,D):-A<B,gcd(B,A).

gcd_list([X],X).
gcd_list([H|T],D):-gcd_list(T,D1),gcd(H,D1,D).
coprime_list(L,P):-subset2(L,P),gcd_list(P,D),D=:=1.

len2([],N):-N.
len2([H|T],N):-len2(T,N1),N1 is N+1.

not_minimal_coprime_list(L,P):-coprime_list(L,P1),len2(P,N),len2(P1,N1),N1<N.
minimal_coprime_list(L,P):-coprime_list(L,P),not(not_minimal_coprime_list(L,P)).

bet2(X,Y,X):-X=<Y.
bet2(X,Y,Z):-X<Y,X1 is X+1,bet2(X1,Y,Z).

divisor(A,B,D):-A mod D=:=0,B mod D=:=0.
common_divisors(A,B,D):-bet2(1,A,D),divisor(A,B,D).
not_greatest_common_div(A,B,D):-common_divisors(A,B,D1),D<D1.
greatest_common_div(A,B,D):-common_divisors(A,B,D),
    not(not_greatest_common_div(A,B,D)).

not_common_divisor_of_list(L,D):-mem2(A,L),A mod D=\=0.
common_divisor_of_list([H|T],D):-bet2(1,H,D),
    not(not_common_divisor_of_list([H|T],D)).

not_greatest_common_divisor_of_list([H|T],D):-common_divisor_of_list([H|T],D1),
    D<D1.
greatest_common_divisor_of_list([H|T],D):-common_divisor_of_list([H|T],D),
    not(not_greatest_common_divisor_of_list([H|T],D)).
