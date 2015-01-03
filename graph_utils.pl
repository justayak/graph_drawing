:- module(graph_utils, [
    areNeighbors/3, 
    edgeToSegment/2, 
    neighbors/2,
    getNodes/2,
    example/1
    ]).

edgeToSegment(edge(A,B,_),(A,B)).

% 
areNeighbors(A,B, G) :-
    member(edge(A,B,_), G),!;
    member(edge(B,A,_), G),!.

neighbors(A,[],G,R,R).
neighbors(A,[H|T],G,R,X) :-
    areNeighbors(A,H,G),
        append(R,H,R),
        neighbors(A,T,G,R),!.
neighbors(A,[H|T],G,R,X) :-
    not(areNeighbors(A,H,G)),
        neighbors(A,T,G,R).


getNodes(G,N):-
    innerGetNodes(G,[],N).

innerGetNodes([],N,N).
innerGetNodes([H|T],A,N):-
    src(H,X),
    write(X),
    member(X,A),
    write('lol'),
    innerGetNodes(T,A,N).
innerGetNodes([H|T],A,N):-
    src(H,X),
    not(member(X,A)),
    write(T),
    append(A,X,L),
    innerGetNodes(T,L,N).


src(edge(A,_,_),A).
dst(edge(_,B,_),B).

example([
    edge(a,b,0.9),
    edge(a,c,0.3),
    edge(a,e,0.3),
    edge(b,c,1.8),
    edge(c,d,0.8),
    edge(d,e,1.5)
]).

/* ==========================
 *       UNIT TEST
 * ==========================*/
:- begin_tests(graph_utils).

compare((A,B),(A,B)).

test(parse) :-
    edgeToSegment(edge(a,b,0.4), S),
    compare(S, (a,b)).

test(neighbor) :-
    areNeighbors(a,b,[edge(a,b,0.3), edge(b,c,0.2)]).

test(notneighbor):-
    not(areNeighbors(a,b,[edge(a,c,2),edge(c,b,4)])).

test(getN):-
    example(G),
    getNodes(G,N).

:- end_tests(graph_utils).
