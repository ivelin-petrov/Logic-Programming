%Graph is usually defined as pair (V,E), where V is a set of vertices and E is a set of edges. 
%The edges can be represented in Prolog as facts:

edge(1, 2).
edge(1, 4).
edge(1, 3).
edge(2, 3).
edge(2, 5).
edge(3, 4).
edge(3, 5).
edge(4, 5).

%To represent the fact that the edges are bi-directional 
%Not a good idea, could lead to an infinite loop
edge(X, Y) :- edge(Y, X).

%The right way
%connected(X, Y) :- edge(X, Y).      These two are equivalent
%connected(X, Y) :- edge(Y, X).      to the bottom line (; == or) 
connected(X, Y) :- edge(X, Y) ; edge(Y, X).

%Path from one node to another

travel(A, B, P, [B|P]) :- connected(A, B).
travel(A, B, Visited, Path) :- connected(A, C), C \== B, \+ member(C, Visited), 
                               travel(C, B, [C|Visited], Path). 

path(A, B, Path) :- travel(A, B, [A], Q), reverse(Q, Path).

%Shortest path from one node to another

minimal([F|R], M) :- min(R, F, M).

% minimal path
min([], M, M).
min([[P, L]|R], [_, M], Min) :- L < M, !, min(R, [P, L], Min). 
min([_|R], M, Min) :- min(R, M, Min).

shortest(A, B, Path, Length) :- setof([P, L], path(A, B, P, L), Set), Set = [_|_], 
                                minimal(Set, [Path, Length]).

% two nodes are connected, if we can walk from one to the other, first seeding the visited list with the empty list
path(A, B) :- walk(A, B, []).           

% we can walk from A to B ... if A is connected to X, and  we haven't yet visited X, and either
% X is the desired destination OR we can get to it from X
walk(A, B, V) :- edge(A, X), not(member(X, V)), (B = X ; walk(X, B, [A|V])).                 