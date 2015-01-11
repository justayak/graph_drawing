:- module(io,[]).
:- use_module(social_network, [renderGraph/4]).
:- initialization(main).

renderNodes([],_).
renderNodes([Segment|Rest],NodesAlreadyUsed):-
    f(Segment,NodeA),
    s(Segment,PointA),
    t(Segment,NodeB),
    q(Segment,PointB),
    printNode(NodeA,PointA,NodesAlreadyUsed,Acc),
    printNode(NodeB,PointB,Acc,Acc2),
    renderNodes(Rest,Acc2).

printNode(Node,Point,NodesAlreadyUsed,NodesAlreadyUsedAcc):-
    (
        member(Node,NodesAlreadyUsed) ->
            NodesAlreadyUsedAcc = NodesAlreadyUsed
        ;
            x(Point,X),
            y(Point,Y),
            write(Node),writeln('['),
            write('label='),writeln(Node),
            write('pos="'),write(X),write(','),write(Y),writeln('!"'),
            writeln(']'),
            append(NodesAlreadyUsed,[Node],NodesAlreadyUsedAcc)
    ).

renderEdges([]).
renderEdges([Segment|Rest]):-
    f(Segment,A),
    t(Segment,B),
    l(Segment,Value),
    write(A),write('--'),write(B),write('[penwidth='),write(Value),writeln(']'),
    renderEdges(Rest).

f((X,_,_,_,_),X).
s((_,X,_,_,_),X).
t((_,_,X,_,_),X).
q((_,_,_,X,_),X).
l((_,_,_,_,X),X).

x(point(X,_),X).
y(point(_,Y),Y).

filter([graph(G)],G).

readFile(Stream,[]):-
    at_end_of_stream(Stream).
readFile(Stream,[X|L]):-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    readFile(Stream,L).

main:-
    %graph2(G), 
    open('graph.txt', read, Str),
    readFile(Str,GIn),
    filter(GIn,G),
    renderGraph(G,10,10,Seg),
    writeln('graph G {'),
    renderNodes(Seg,[]),
    renderEdges(Seg),
    writeln('}'),
    halt.
