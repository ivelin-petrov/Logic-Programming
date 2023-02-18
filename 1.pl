nat(c).
nat(s(X)):-nat(X).

sum(X,c,X).
sum(X,s(Y),s(Z)):-sum(X,Y,Z).

app([],L2,L2).
app([H|T],L2,[H|L]):-app(T,L2,L).

mem2(X,L):-app(Z1,[X|Z2],L).

len([],N):-N is 0.
len([H|T],N):-len(T,N1),N is N1+1.

permute([],[]).
permute([H|T],P):-permute(T,PT),app(PT1,PT2,PT),app(PT1,[H|PT2],P).

incr([]).
incr([X]).
incr([X,Y|T]):- X=<Y,incr([Y|T]).

not_incr(S):- app(L1,[X,Y|L2],S), X>Y.
incr2(S):- not(not_incr(S)).

sort2(L,S):- permute(L,S), incr(S).
sort3(L,S):- permute(L,S), incr2(S).

split([],[],[]).
split([X],[],[X]).
split([X,Y|L],[Y|Even],[X|Odd]):-split(L,Even,Odd).

merge2([],S2,S2).
merge2(S1,[],S1).
merge2([H1|S1],[H2|S2],[H1|S]):-H1=<H2,merge2(S1,[H2|S2],S).
merge2([H1|S1],[H2|S2],[H2|S]):-H2<H1, merge2([H1|S1],S2,S).
merge_sort([],[]).
merge_sort([X],[X]).
merge_sort([X,Y|L],S):-split([X,Y|L],Even,Odd),
    merge_sort(Even,EvenS),merge_sort(Odd,OddS),merge2(EvenS,OddS,S).

len3([],0).
len3([H|T],N):-len3(T,N1),N is N1+1.

permute2([],[]).
permute2([H|T],P):-permute2(T,PT),app(L1,L2,PT),app(L1,[H|L2],P).

subset3([],[]).
subset3([H|T],[H|S]):-subset3(T,S).
subset3([H|T],S):-subset3(T,S).
