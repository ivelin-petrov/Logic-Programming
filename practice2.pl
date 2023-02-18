%task1
count([],_,0).
count([X|T],H,N):-count(T,H,N),X\=H.
count([H|T],H,N):-count(T,H,N1),N is N1+1.

app([],L2,L2).
app([H|L1],L2,[H|L]):-app(L1,L2,L).

mem(X,L):-app(L1,[X|L2],L).

mostFreq(L,X,N):-mem(X,L),count(L,X,N1),not((mem(Y,L),count(L,Y,N2),N1<N2)).

leastFreq(L,X,N):-mem(X,L),count(L,X,N1),not((mem(Y,L),count(L,Y,N2),N1>N2)).

diameter(L,D):-mostFreq(L,_,Max),leastFreq(L,_,Min),D is Max-Min.

q(L):-not((mem(X,L),mem(Y,L),X\=Y,diameter(X,DX),diameter(Y,DY),DX==DY)).

%task2

permut([],[]).
permut([H|T],P):-permut(T,PT),app(A,B,PT),app(A,[H|B],P).

bet(X,Y,X):-X=<Y.
bet(X,Y,Z):-X<Y,X1 is X+1,bet(X1,Y,Z).

ellipse(X,Y,A,B):-Am is -A,Bm is -B,bet(Am,A,X),bet(Bm,B,Y),X^2/A^2+Y^2/B^2<1.

nat(0).
nat(N):-nat(K),N is K+1.

p(N):-nat(K),bet(0,K,A),bet(0,K,B),bet(0,K,C),N is 103*(A+3)^2*(B+2*A*B+1)^3*(C+5)^4.

listN([],0,K).
listN([A|Y],N,K):-N1 is N-1,listN(Y,N1,K),bet(0,K,A).

list(X):-nat(K),bet(0,K,N),listN(X,N,K).

fibo1(0,0).
fibo1(1,1).
fibo1(N,F):-A is N-1,B is N-2,fibo1(A,C),fibo1(B,D),F is C+D.

fibo2(0,0,1).
fibo2(N,B,C):-N1 is N-1,fibo2(N1,A,B),C is A+B.

fibo3(0,1).
fibo3(B,C):-fibo3(A,B),C is A+B.
fibo(A):-fibo3(A,_).

tribo3(0,1,2).
tribo3(B,C,D):-tribo3(A,B,C),D is A+B+C.
tribo(A):-tribo3(A,_,_).

reverse([],R,R).
reverse([H|T],F,R):-reverse(T,[H|F],R).
reverse(L,R):-reverse(L,[],R).

split([],[],[]).
split([H|T],[H|Y],R):-split(T,Y,R).
split([H|T],Y,[H|R]):-split(T,Y,R).

fst([A,B],[A]).
snd([A,B],[B]).

get_first([],R,R).
get_first([H|T],F,R):-fst(H,A),app(F,A,F1),get_first(T,F1,R).
get_first(L,R):-get_first(L,[],R).

get_second([],R,R).
get_second([H|T],F,R):-snd(H,B),app(F,B,F1),get_second(T,F1,R).
get_second(L,R):-get_second(L,[],R).

max(A,B,A):-A>B.
max(A,B,B):-B>=A.

is_not_prime(N):-N1 is N-1,bet(2,N1,R),N mod R =:= 0.
is_prime(N):-not(is_not_prime(N)).

gen_list(0,L,L).
gen_list(N,L,R):-N>0,N1 is N-1,gen_list(N1,[N|L],R).
gen_list(N,L):-gen_list(N,[],L).

subset2([],[]).
subset2([H|T],[H|R]):-subset2(T,R).
subset2([H|T],R):-subset2(T,R).

len2([],0).
len2([H|T],S):-len2(T,S1),S is S1+1.

divisor(N,D):-bet(1,N,D),0 is N mod D.
prime_divisor(N,D):-divisor(N,D),is_prime(D).

gen_divisors(N,K,L,L1):-N=:=K,app(L,[N],L1).
gen_divisors(N,K,M,L):-N>K,0 is N mod K,K1 is K+1,app(M,[K],M1),gen_divisors(N,K1,M1,L).
gen_divisors(N,K,M,L):-N>K,not(0 is N mod K),K1 is K+1,gen_divisors(N,K1,M,L).
gen_divisors(N,L):-gen_divisors(N,1,[],L).

filter_prime(L,0,R,R).
filter_prime([H|T],S,R,L):-S>0,S1 is S-1,is_prime(H),app(R,[H],R1),filter_prime(T,S1,R1,L).
filter_prime([H|T],S,R,L):-S>0,S1 is S-1,not(is_prime(H)),filter_prime(T,S1,R,L).
filter_prime(L,S,R):-filter_prime(L,S,[],R).
gen_prime_divisors(N,R):-gen_divisors(N,L),len2(L,S),filter_prime(L,S,R).

pnp(N):-gen_divisors(N,L1),gen_prime_divisors(N,L2),L1=L2.

aperiod([0,Z]):-random_member(Z,[0,1]).
aperiod([X,Z]):-aperiod([Y,_]),X is Y+1,random_member(Z,[0,1]).

nat2(C,C).
nat2(C,X):-nat2(C,Y),X is Y+1.

nat3(F,T,F).
nat3(F,T,X):-nat3(F,T,Y),X is Y+1,Y<T.
