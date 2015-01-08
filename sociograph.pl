:- module(sociograph, [
    sociograph/1,
    min/3,
    degToPoint/5,
    renderGraph/4
]).
:- use_module(lib/plot_graph, [segments_plot/1]).
:- use_module(graph_utils, [distinctCliques/2, example/1,example2/1]).

sociograph(a).

/* ===============================
 * RENDER_GRAPH
 * G: Graph
 * W: Width
 * H: Height
 * Segments: Return-Value: List of segments with:
 * [wSegment(point(X1,Y1),point(X2,Y2),WEIGHT,DIRECTED)]
 * =============================== */ 
renderGraph(G,W,H,Segments) :-
    distinctCliques(G,DistinctMaxCliques),
    defineCircleParameters(DistinctMaxCliques,W,H,Circle),
    renderGraph(DistinctMaxCliques,Circle,0,[],Segments)
.

renderGraph([],_,_,Segments,Segments).
renderGraph([Clique|Rest],Circle,I,Acc,Segments) :-
    Ipp is I+1,
    alpha(Circle,Alpha),
    AlphaC is Alpha * I,
    innerRadius(Circle,R),
    x(Circle,X),
    y(Circle,Y),
    degToPoint(X,Y,AlphaC,R,Point),
    px(Point,Px),
    py(Point,Py),
    calculateCliqueCircle(Clique,Circle,Px,Py,CliqueSegments),
    append(Acc,CliqueSegments,Acc2),
    renderGraph(Rest,Circle,Ipp,Acc2,Segments).


calculateCliqueCircle(Clique,Circle,X,Y,Result) :-
    length(Clique,N),
    Alpha is 360/N,
    calculateCliqueCircle(Clique,Circle,Alpha,X,Y,0,[],Points),
    segmentate(Points,[],Result).

calculateCliqueCircle([],_,_,_,_,_,Result,Result).
calculateCliqueCircle([_|Rest],Circle,Alpha,X,Y,I,Acc,Result) :-
    Ipp is I+1,
    AlphaN is I * Alpha,
    cliqueRadius(Circle,R),
    degToPoint(X,Y,AlphaN,R,Point),
    append(Acc,[Point],Acc2),
    calculateCliqueCircle(Rest,Circle,Alpha,X,Y,Ipp,Acc2,Result).


segmentate([],Segments,Segments).
segmentate([A|Rest],Acc,Segments) :-
    createSegments(A,Rest,[],PartialSegments),
    append(Acc,PartialSegments,Acc2),
    segmentate(Rest,Acc2,Segments).

createSegments(_,[],Segments,Segments).
createSegments(Node,[NextNode|Rest],Acc,Segments):-
    append(Acc,[segment(Node,NextNode)],Acc2),
    createSegments(Node,Rest,Acc2,Segments).

/* ===============================
 * DEFINE_RADIUS
 * DistinctMaxCliques
 * W: Width
 * H: Height
 * Circle: (R,R_Multiplier,SCircleDistance)
 *    R: Radius of the inner circle
 *    R_Multiplier: Multiplier by which
 *      the number of nodes in a clique
 *      is multiplied to determine the
 *      cliques circle's radius
 *    SCircleDistance: Arc-Distance between
 *      sub circles on the main circle
 * =============================== */ 
defineCircleParameters(DistinctMaxCliques,W,H, Circle):-
    length(DistinctMaxCliques,N),
    min(W,H,S),
    X is W/2,
    Y is H/2,
    Alpha is 360/N,
    HalfAlpha is Alpha/2,
    degToRad(HalfAlpha,HalfAlphaRad),
    RB is S / (2 * (1 + sin(HalfAlphaRad))),
    RS is RB * sin(HalfAlphaRad),

    RSReduced is RS * 0.9, % reduce the clique circle a little so they arent too close
    Circle = (Alpha,RB,RSReduced,X,Y)
    .

alpha((A,_,_,_,_),A).
innerRadius((_,R,_,_,_),R).
cliqueRadius((_,_,R,_,_),R).
x((_,_,_,X,_),X).
y((_,_,_,_,Y),Y).
px(point(X,_),X).
py(point(_,Y),Y).

/* ============== *
 *   HELPER FUNC  *
 * ============== */
set(A,A).
min(A,B,Result):-
    (
        A > B ->
            set(B,Result)
        ;
            set(A,Result)
    ).

% calculates the point that is specified by another point and
% a degree and a radius
degToPoint(X,Y,  0,R,Point):- XR is X + R, Point = point(XR,Y),!.
degToPoint(X,Y, 90,R,Point):- YR is Y - R, Point = point(X,YR),!.
degToPoint(X,Y,180,R,Point):- XR is X - R, Point = point(XR,Y),!.
degToPoint(X,Y,360,R,Point):- YR is Y + R, Point = point(X,YR),!.
degToPoint(X,Y,Alpha,R,Point):-
    degToRad(Alpha,AlphaRad),
    Xs is X + R * cos(AlphaRad),
    Ys is Y + R * sin(AlphaRad),
    Point = point(Xs,Ys),!.

degToRad(Deg,Rad):-
    Rad is Deg * (pi / 180).

/* ============== *
 *   UNIT  TEST   *
 * ============== */
:- begin_tests(sociograph).

ex([
    segment(point(1,3),point(6,10)),
    segment(point(3,0),point(6,10))
]).

test(first) :-
    graph_utils:example2(G),
    graph_utils:distinctCliques(G,C),
    defineCircleParameters(C,800,600,Ci),
    writeln(Ci).

test(plot) :-
    ex(Ss),
    segments_plot(Ss).

test(plot2):-
    graph_utils:example2(G),
    renderGraph(G,800,600,Ss),
    writeln(Ss),
    segments_plot(Ss).

:- end_tests(sociograph).
