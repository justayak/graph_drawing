:- module(graph_utils, []).

edgeToSegment(edge(A,B,_),(A,B)).

areNeighbors(A,B, G) :-
    member(edge(A,B,_), G),!;
    member(edge(B,A,_), G),!.


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

:- end_tests(graph_utils).
