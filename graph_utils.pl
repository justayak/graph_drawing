:- module(graph_utils, [
    areNeighbors/3, 
    edgeToSegment/2, 
    getNeighbors/3,
    getNodes/2,
    example/1,
    insertIntoSet/3,
    distinctCliques/2,
    powerset/2,
    sortByListSize/2,
    isClique/2,
    delete/3,
    deleteIfContains/3
    ]).

edgeToSegment(edge(A,B,_),(A,B)).

/* ==========================
 * ARE_NEIGHBORS/3
 * ==========================*/
areNeighbors(A,B, G) :-
    member(edge(A,B,_), G),!;
    member(edge(B,A,_), G),!.

/* ==========================
 * GET_NEIGHBORS/3
 *        A = The Node
 *        G = The Graph
 *        N = Result
 * This function has POOR performance!
 * ==========================*/
getNeighbors(A,G,N):-
    getNeighborsRec(A,G,[],N),!.


getNeighborsRec(_,[],N,N).
getNeighborsRec(A,[H|T],Acc,N):-
    src(H,X),
    dst(H,Y),
    (
        A == X ->
            insertIntoSet(Y,Acc,AccPlus)    
        ; A == Y ->
            insertIntoSet(X,Acc,AccPlus)
        ;
            set(Acc,AccPlus)
    ),
    getNeighborsRec(A,T,AccPlus,N).

/* ==========================
 * FIND_DISTINCT_MAXIMUM_CLIQUES/2
 * ==========================*/
distinctCliques(G,MaxCliques):-
    getNodes(G,Nodes),
    powerset(Nodes,Pow),
    sortByListSize(Pow, AllSubgraphs), 
    distinctCliques(G,AllSubgraphs,[],MaxCliques),!
    .

distinctCliques(_,[],M,M).
distinctCliques(G,[H|T],Acc,MaxCliques):-
    (
        isClique(H,G) ->
            deleteIfContains(H, T, Rest),
            append(Acc,[H],Acc2),
            distinctCliques(G,Rest,Acc2,MaxCliques)
        ;
            distinctCliques(G,T,Acc,MaxCliques)
    ).


isClique(SubGraph, G) :-
    length(SubGraph, Len),
    (
        Len > 0 ->
            isCliqueRec(SubGraph,SubGraph,G),!
    ).

isCliqueRec([],_,_).
isCliqueRec([X|R],SubGraph,G):-
    isCliqueRecSingle(X,SubGraph,G),
    isCliqueRec(R,SubGraph,G).

isCliqueRecSingle(_,[],_).
isCliqueRecSingle(X,[A|Rest],G):-
    (
        X == A ->
            isCliqueRecSingle(X,Rest,G)
        ;
            areNeighbors(X,A,G),
            isCliqueRecSingle(X,Rest,G)
    ).

/* ==========================
 * GET_NODES/2
 * ==========================*/
getNodes(G,N):-
    getNodesRec(G,[],N),!.

% inner helper function
getNodesRec([],N,N).
getNodesRec([H|T],A,N):-
    src(H,X),
    dst(H,Y),
    insertIntoSet(X,A,L),
    insertIntoSet(Y,L,M),
    getNodesRec(T,M,N).

/* ==========================
 *  GENERAL PURPOSE HELPERS
 * ==========================*/

listIsBigger(>, ListA, ListB):-
    length(ListA, A),
    length(ListB, B),
    A=<B.
listIsBigger(<, ListA, ListB):-
    length(ListA, A),
    length(ListB, B),
    A>B.

sortByListSize(Xs,S):-
     predsort(listIsBigger,Xs,S),!.
    


powerset(X,Y) :- bagof(S,subseq(S,X),Y).

subseq([],[]).
subseq([],[_|_]).
subseq([X|Xs],[X|Ys]) :- subseq(Xs,Ys).
subseq([X|Xs],[_|Ys]) :- append(_, [X|Zs],Ys), subseq(Xs,Zs).

insertIntoSet(A,Set,Result):-
    (
        member(A,Set) ->
            set(Set,Result)
        ;
            append(Set,[A], Result)
    ).

set(A,A).

src(edge(A,_,_),A).
dst(edge(_,B,_),B).

delete(X,[X|T], T).
delete(X, [H|T], [H|S]):-
    delete(X,T,S).
permuation([],[]).
permuation([H|T], R):-
    permutation(T,X), delete(H,R,X).


deleteIfContains(List, PowSet, R):-
    deleteIfContainsRec(List, PowSet, R), !.

deleteIfContainsRec([], P, P).
deleteIfContainsRec([H|T], P, R):-
    deleteIfContainsRecSingle(H, P, [], P2),
    deleteIfContainsRec(T, P2, R).

deleteIfContainsRecSingle(_, [], R,R).
deleteIfContainsRecSingle(E, [H|T],Acc, Result):-
    (
        member(E, H) ->
            deleteIfContainsRecSingle(E, T, Acc, Result)
        ;
            append(Acc, [H], Acc2),
            deleteIfContainsRecSingle(E, T, Acc2, Result)
    ).

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

test(getNeighbors):-
    example(G),
    getNeighbors(a,G,Na),
    getNeighbors(e,G,Ne),
    getNeighbors(c,G,Nc),
    getNeighbors(b,G,Nb),
    permutation(Na,[b,c,e]),
    permutation(Ne,[a,d]),
    permutation(Nc,[a,b,d]),
    permutation(Nb,[a,c]).

test(getNodes):-
    example(G),
    getNodes(G,N),
    permutation(N,[a,b,c,d,e]).

test(isClique):-
    example(G),
    isClique([a,b,c],G),
    isClique([e,d],G).

test(isNotClique):-
    example(G),
    not(isClique([a,d],G)),
    not(isClique([a,b,c,e],G)),
    not(isClique([a,b,c,d,e],G)).

test(clique):-
    example(G),
    distinctCliques(G,N),
    N == [[a,b,c],[e,d]].


:- end_tests(graph_utils).
