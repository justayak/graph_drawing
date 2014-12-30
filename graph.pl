:- module(graph, [areNeighbors/3, example/1, nodes/2]).
:- use_module(lib/plot_graph, [segments_plot/1]).

edgeToSegment(edge(A,B,_),(A,B)).

areNeighbors(A,B, G) :- 
    member(edge(A,B,_), G),!;
    member(edge(B,A,_), G),!.

% Get all the nodes of a graph as list
nodes(L, Nodes) :- nodes(L,[],Nodes).
nodes([],A,A).
nodes([edge(A,B,_)|R],Acc,Nodes) :- 
    not(member(A,Acc)), 
    not(member(B,Acc)),
    nodes(R,[A,B|Acc], Nodes).
nodes([edge(A,B,_)|R],Acc,Nodes) :- 
    not(member(A,Acc)),
    member(B,Acc),
    nodes(R,[A|Acc], Nodes).
nodes([edge(A,B,_)|R],Acc,Nodes) :- 
    member(A,Acc),
    not(member(B,Acc)),
    nodes(R,[B|Acc], Nodes).
nodes([edge(A,B,_)|R],Acc,Nodes) :- 
    member(A,Acc),
    member(B,Acc),
    nodes(R,Acc, Nodes).

% no pivoting!
bronKerbosch(R,[],[]).
bronKerbosch(R, [V|Rest], X):-
    bronKerbosch([V|R], 


% HELPER FUNCTIONS

example([
    edge(a,b,0.9),
    edge(a,c,0.3),
    edge(a,e,0.3),
    edge(b,c,1.8),
    edge(c,d,0.8),
    edge(d,e,1.5)
]).

/* ======================= *
 *       UNIT TEST         *
 * ======================= */
:- begin_tests(graph).


compare((A,B),(A,B)).

%parse the edge to a tuple
test(parse) :-
    edgeToSegment(edge(a,b,0.5), S),
    compare(S, (a,b)).

test(neighbor1) :-
    example(A),
    areNeighbors(a,b,A).

:- end_tests(graph).
