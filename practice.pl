app([],L2,L2).
app([H|L1],L2,[H|L]):-app(L1,L2,L).

len([],N):-N is 0.
len([H|T],N):-len(T,N1),N is N1+1.

mem2(X,L):-app(A1,[X|A2],L).

permut([],[]).
permut([H|T],P):-permut(T,PT),append(A1,A2,PT),append(A1,[H|A2],P).

subset2([],[]).
subset2([H|L],[H|S]):-subset2(L,S).
subset2([H|L],S):-subset2(L,S).

naturals(0).
naturals(X):-naturals(Y),X is Y+1.

bet2(X,Y,X):-X=<Y.
bet2(X,Y,Z):-X<Y,X1 is X+1,bet2(X1,Y,Z).

gcd(A,0,A).
gcd(A,B,C):-B>0,A>=B,A1 is A-B,gcd(A1,B,C).
gcd(A,B,C):-B>A,gcd(B,A,C).

%task1
gen_llist(0,L,L).
gen_llist(N,R,L):-N>0,N1 is N-1,gen_llist(N1,[[_,_]|R],L).

app2([],L2,L2).
app2([[A,B]|L1],L2,[[A,B]|L]):-app2(L1,L2,L).

mem3(X,L):-app2(L1,[[X,_]|L2],L).

snd_check([]).
snd_check([[A,B]|L]):-number(B),snd_check(L).

fst_check([]).
fst_check(L):-app2(L1,[[A,_]|L2],L),((mem3(A,L1)) ; mem3(A,L2)).

fst([A,B],[A]).
snd([A,B],[B]).

get_first([],R,R).
get_first([H|T],F,R):-fst(H,A),app(F,A,F1),get_first(T,F1,R).
get_first(L,R):-get_first(L,[],R).

get_second([],R,R).
get_second([H|T],F,R):-snd(H,B),app(F,B,F1),get_second(T,F1,R).
get_second(L,R):-get_second(L,[],R).

nivka(N,L):-gen_llist(N,[],L),not(fst_check(L)),snd_check(L).
gen_nivka(L):-len(L,N),nivka(N,L).

sum([],R,R).
sum([H|T],F,R):-F1 is F+H,sum(T,F1,R).
sum(L,R):-sum(L,0,R).

equal(A,B):-subset2(A,B),subset2(B,A).

intersec([],B,R,R).
intersec(A,[],R,R).
intersec([H1|A],[H2|B],F,R):-mem2(H1,B),mem2(H2,A),
    app(F,[A,B],F1),
    intersec(A,B,F1,R).
intersec([H1|A],[H2|B],F,R):-not(mem2(H1,B)),mem2(H2,A),
    app(F,A,F1),
    intersec(A,B,F1,R).
intersec([H1|A],[H2|B],F,R):-mem2(H1,B),not(mem2(H2,A)),
    app(F,B,F1),
    intersec(A,B,F1,R).
intersec([H1|A],[H2|B],F,R):-not(mem2(H1,B)),not(mem2(H2,A)),
    intersec(A,B,F,R).
intersec(A,B,R):-intersec(A,B,[],R).

% L=[[a,1],[c,0]],subset2(L,A),subset2(L,B),intersec(A,B,I),get_first(A,F1),get_first(B,F2),get_first(I,F3),not(equal(F1,F2)),F3\=[],get_second(A,S1),get_second(B,S2),get_second(I,S3),sum(S1,R1),sum(S2,R2),sum(S3,R3),R3=:=R1*R2.

indepen(L):-gen_nivka(L),subset2(L,A),subset2(L,B),
    intersec(A,B,I),
    get_first(A,F1),get_first(B,F2),get_first(I,F3),
    not(equal(F1,F2)),F3\=[],
    get_second(A,S1),get_second(B,S2),get_second(I,S3),
    sum(S1,R1),sum(S2,R2),sum(S3,R3),R3=:=R1*R2.

