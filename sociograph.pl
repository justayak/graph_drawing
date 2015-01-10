:- module(sociograph, [
    degToPoint/5,
    renderGraph/4
]).
:- use_module(lib/plot_graph, [segments_plot/1]).
:- use_module(graph_utils, [
    distinctCliques/2, 
    example/1,
    example2/1,
    findInterconnections/3,
    getNeighbors/3    
]).

/* ===============================
 * RENDER_GRAPH
 * G: Graph
 * W: Width
 * H: Height
 * Segments: Return-Value: List of segments with:
 * [wSegment(point(X1,Y1),point(X2,Y2),WEIGHT,DIRECTED)]
 * =============================== */ 
renderGraph(G,W,H,Segments) :-
    min(W,H,S),
    distinctCliques(G,DistinctMaxCliques),
    defineCircleParameters(DistinctMaxCliques,W,H,Circle),
    %renderGraph(DistinctMaxCliques,Circle,0,[],CliqueSegments,[],Lookup),
    renderGraphHeuristics(G,DistinctMaxCliques,Circle,0,[],CliqueSegments,[],Lookup),
    findInterconnections(G,DistinctMaxCliques,Inter),
    segmentsToPositions(Inter,Lookup,InterSegments),
    %append(CliqueSegments,InterSegments,Segments).
    append(CliqueSegments,InterSegments,Temp),
    append(Temp,[
        segment(point(0,0),point(0,0)),
        segment(point(S,S),point(S,S))
    ],Segments).

/* ===========================================================================
 *  N O  H E U R I S T I C ! !
 *  Lookup: [(a,point(X,Y)), (b,point(X,Y)), ...]
 */
renderGraph([],_,_,Segments,Segments,Lookup,Lookup).
renderGraph([Clique|Rest],Circle,I,Acc,Segments,LookupAcc,Lookup) :-
    Ipp is I+1,
    alpha(Circle,Alpha),
    AlphaC is Alpha * I,
    innerRadius(Circle,R),
    x(Circle,X),
    y(Circle,Y),
    degToPoint(X,Y,AlphaC,R,Point),
    px(Point,Px),
    py(Point,Py),
    random(0.0,360.0,RandomDeg),
    calculateCliqueCircle(Clique,Circle,Px,Py,0,CliqueSegments,CircleLookup),
    append(Acc,CliqueSegments,Acc2),
    append(LookupAcc,CircleLookup,LookupAcc2),
    renderGraph(Rest,Circle,Ipp,Acc2,Segments,LookupAcc2,Lookup).


calculateCliqueCircle(Clique,Circle,X,Y,RandomDeg,Result,Lookup) :-
    length(Clique,N),
    Alpha is 360/N,
    calculateCliqueCircle(Clique,Circle,Alpha,X,Y,0,RandomDeg,[],Points,[],Lookup),
    segmentate(Points,[],Result).

calculateCliqueCircle([],_,_,_,_,_,_,Result,Result,Lookup,Lookup).
calculateCliqueCircle([Node|Rest],Circle,Alpha,X,Y,I,RandomDeg,Acc,Result,LookupAcc,Lookup) :-
    Ipp is I+1,
    AlphaT is I * Alpha,
    addDeg(AlphaT,RandomDeg,AlphaN),
    cliqueRadius(Circle,R),
    degToPoint(X,Y,AlphaN,R,Point),
    append(Acc,[Point],Acc2),
    append(LookupAcc,[(Node,Point)],LookupAcc2),
    calculateCliqueCircle(Rest,Circle,Alpha,X,Y,Ipp,RandomDeg,Acc2,Result,LookupAcc2,Lookup).
/*============================================================================*/

/* =======================================
 * RENDER_GRAPH_HEURISTICS
 * =======================================*/

renderGraphHeuristics(_,[],_,_,Segments,Segments,Lookup,Lookup).
renderGraphHeuristics(G,[Clique|Rest],Circle,I,Acc,Segments,LookupAcc,Lookup):-
    Ipp is I + 1,
    alpha(Circle,Alpha),
    AlphaC is Alpha * I,
    innerRadius(Circle,R),
    x(Circle,X),
    y(Circle,Y),
    degToPoint(X,Y,AlphaC,R,Point),
    px(Point,Px),
    py(Point,Py),
    sortByNeighborCount(G,Clique,SortedClique),
    calculatePointsForClique(Clique,Circle,Px,Py,Points),
    sortByDistanceToCenter(X,Y,Points,SortedPoints),
    createLookup(SortedClique,SortedPoints,[],TempLookup),
    writeln(TempLookup),
    append(LookupAcc,TempLookup,LookupAcc2),
    segmentate(Points,[],Temp),
    append(Acc,Temp,Acc2),
    renderGraphHeuristics(G,Rest,Circle,Ipp,Acc2,Segments,LookupAcc2,Lookup).

%============================
%Lookup = [(a,point(...)),(b,point(...)),...]
createLookup([],[],Lookup,Lookup).
createLookup([Node|Clique],[Point|Points],Acc,Lookup):-
    append(Acc,[(Node,Point)],Acc2),
    createLookup(Clique,Points,Acc2,Lookup).

%============================
calculatePointsForClique(Clique,Circle,X,Y,Result) :-
    length(Clique,N),
    Alpha is 360/N,
    cliqueRadius(Circle,R),
    calculatePointsForClique(Clique,Alpha,R,X,Y,0,[],Result)
    .

calculatePointsForClique([],_,_,_,_,_,Acc,Acc).
calculatePointsForClique([N|Rest],Alpha,R,X,Y,I,Acc,Result):-
    Ipp is I + 1,
    AlphaC is Alpha * I,
    degToPoint(X,Y,AlphaC,R,Point),
    append(Acc,[Point],Acc2),
    calculatePointsForClique(Rest,Alpha,R,X,Y,Ipp,Acc2,Result).

/*SORT BY DISTANCE TO CENTER*/
sortByDistanceToCenter(X,Y,Points,Sorted):-
    Center = point(X,Y),
    sortPrepD(Center,Points,[],Temp),
    predsort(checkSecondParam,Temp,Temp2),
    reverse(Temp2,Temp3),
    sortUnwrap(Temp3,[],Sorted),!.

sortPrepD(_,[],Acc,Acc).
sortPrepD(Center,[Point|Rest],Acc,Result):-
    euclideanDistance(Center,Point,Distance),
    append(Acc,[(Point,Distance)],Acc2),
    sortPrepD(Center,Rest,Acc2,Result).

/*SORT BY NEIGHBORS*/
sortByNeighborCount(G,Clique,Sorted) :- 
    sortPrepN(G,Clique,[],Temp),
    predsort(checkSecondParam,Temp,Temp2),
    sortUnwrap(Temp2,[],Sorted),!.

checkSecondParam(>,A,B):-
    p2(A,Ac),
    p2(B,Bc),
    Ac=<Bc.
checkSecondParam(<,A,B):-
    p2(A,Ac),
    p2(B,Bc),
    Ac>Bc.

p2((_,C),C).

sortPrepN(_,[],Acc,Acc).
sortPrepN(G,[Node|Rest],Acc,Result) :-
    getNeighbors(Node,G,N),
    length(N,Count),
    append(Acc,[(Node,Count)],Acc2),
    sortPrepN(G,Rest,Acc2,Result)
    .

sortUnwrap([],Acc,Acc).
sortUnwrap([(Node,_)|Rest],Acc,Result):-
    append(Acc,[Node],Acc2),
    sortUnwrap(Rest,Acc2,Result).

/*============================================================================*/

segmentate([],Segments,Segments).
segmentate([A|Rest],Acc,Segments) :-
    createSegments(A,Rest,[],PartialSegments),
    append(Acc,PartialSegments,Acc2),
    segmentate(Rest,Acc2,Segments).

createSegments(_,[],Segments,Segments).
createSegments(Node,[NextNode|Rest],Acc,Segments):-
    append(Acc,[segment(Node,NextNode)],Acc2),
    createSegments(Node,Rest,Acc2,Segments).

addDeg(Deg1,Deg2,Result):-
    (
        (Deg1 + Deg2) > 360 ->
            Result is abs(Deg1 - Deg2)
        ;
            Result is Deg1 + Deg2
    ).
/* ===============================
 *    Segments to Positions
 *    Segments: [segment(a,b), segment(b,c), ...]
 *    Lookup: [(a,point(...)), (b,point(..)))]
 * =============================== */
segmentsToPositions(Segments,Lookup,Result) :-
    segmentsToPositions(Segments,Lookup,[],Result),!.


segmentsToPositions([],_,Positions,Positions).
segmentsToPositions([Seg|Rest],Lookup,Acc,Positions):-
    segA(Seg,A),
    segB(Seg,B),
    findPoint(A,Lookup,PA),
    findPoint(B,Lookup,PB),
    append(Acc,[segment(PA,PB)],Acc2),
    segmentsToPositions(Rest,Lookup,Acc2,Positions)
    .

segA(segment(A,_),A).
segB(segment(_,B),B).

findPoint(Node,[Current|Rest],Position):-
    key(Current,OtherNode),
    (
        Node == OtherNode ->
            point(Current,Position)
        ;
            findPoint(Node,Rest,Position)
    ).


key((Node,_),Node).
point((_,Point),Point).

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

    RSReduced is RS * 0.7, % reduce the clique circle a little so they arent too close
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

euclideanDistance(point(X1,Y1),point(X2,Y2),Distance):-
    Distance is sqrt(
        (X1-X2)*(X1-X2) +
        (Y1-Y2)*(Y1-Y2)
    ).

/* ============== *
 *   UNIT  TEST   *
 * ============== */
:- begin_tests(sociograph).

ex([
    segment(point(1,3),point(6,10)),
    segment(point(3,0),point(6,10))
]).

testP1(point(10,0)).
testP2(point(40,0)).

test(distance):-
    testP1(P1),
    testP2(P2),
    euclideanDistance(P2,P1,Distance),
    euclideanDistance(P1,P2,Distance2),
    Distance == Distance2,
    Distance == 30.0.

test(first) :-
    graph_utils:example2(G),
    graph_utils:distinctCliques(G,C),
    defineCircleParameters(C,600,600,Ci),
    writeln(Ci).

test(plot) :-
    graph_utils:example(G),
    renderGraph(G,100,100,Ss),
    writeln('calc done!'),
    segments_plot(Ss).

test(plot2):-
    graph_utils:example2(G),
    renderGraph(G,60,60,Ss),
    writeln('calc done!'),
    segments_plot(Ss).

%sortByNeighborCount(G,Clique,Sorted) :- 
test(sortByNeighborCount):-
    graph_utils:example(G),
    distinctCliques(G,[Clique|_]),
    sortByNeighborCount(G,Clique,Sorted),
    Sorted == [c,a,b].

%calculatePointsForClique(Clique,Circle,X,Y,Result) :-
test(heuristicsCalcPoints):-
    graph_utils:example(G),
    distinctCliques(G,Cliques),
    defineCircleParameters(Cliques,100,100,Circle),
    [Clique|_] = Cliques,
    calculatePointsForClique(Clique,Circle,50,50,Result),
    writeln(Result),
    sortByDistanceToCenter(50,50,Result,Sorted),
    writeln(Sorted)
    .

:- end_tests(sociograph).
