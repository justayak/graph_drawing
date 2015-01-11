:- module(io,[]).
:- use_module(sociograph, [graphizRenderGraph/4]).
:- initialization(main).

% Example graph
g(
    [
        (a,point(0,0),b,point(10,10),1),
        (a,point(0,0),c,point(10,0),1.5),
        (c,point(10,0),d,point(0,10),2)
    ]
).

graph([
    edge(a,b,0.9),
    edge(a,c,0.3),
    edge(a,e,0.3),
    edge(b,c,1.8),
    edge(c,d,0.8),
    edge(d,e,1.5)
]).

graph2([
    edge(a,b,1.3),
    edge(a,c,2),
    edge(a,d,2),
    edge(b,c,0.9),
    edge(b,d,0.7),
    edge(c,d,2),

    edge(e,f,1.2),
    edge(e,g,1),
    edge(e,h,1),
    edge(f,g,2),
    edge(f,h,1.2),
    edge(g,h,0.2),

    edge(i,j,1.2),
    edge(i,l,0.9),
    edge(i,k,1.2),
    edge(j,l,2),
    edge(j,k,1.8),
    edge(l,k,0.4),

    edge(p,q,1.5),
    edge(p,r,1.4),
    edge(q,r,1.5),

    edge(m,n,2),
    edge(m,o,2),
    edge(o,n,2),

    edge(a,s,0.2),
    edge(n,s,0.4),
    edge(f,s,0.1),

    edge(d,g,0.4),
    edge(d,q,1.5),
    edge(d,i,1.4),

    edge(q,i,2),
    edge(i,m,1.2),
    edge(j,m,0.3),
    edge(g,m,0.5),
    edge(h,m,0.3)

]).

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

main:-
    graph2(G), 
    graphizRenderGraph(G,10,10,Seg),
    writeln('graph G {'),
    renderNodes(Seg,[]),
    renderEdges(Seg),
    writeln('}'),
    halt.
