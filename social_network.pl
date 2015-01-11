/* ============================================================ *
 *  Rendering a Social Network
 * ============================================================ */
:-module(social_network, [
		renderGraph/4
	]).

/* ============================================================
 * RENDER_GRAPH
 * This function is supposed to be used in conjunction with 
 * graphiz instead of gnuplot
 * G: Graph expl: [edge(a,b,0.1), edge(a,c,0.2),...]
 * W: Width
 * H: Height
 * Segments: Return Value:
 *      [
 *          (a,point(12,11),b,point(23,11,0.1),
 *          (a,point(12,11),c,point(1,32),0.2),
 *          ...
 *      ]
 * ============================================================ */
renderGraph(G,W,H,Segments) :-
    distinctCliques(G,DistinctMaxCliques),
    %shuffle(DistinctMaxCliques,ShuffledCliques),
    ShuffledCliques = DistinctMaxCliques,
    defineCircleParameters(ShuffledCliques,W,H,Circle),
    renderGraphHeuristics(G,ShuffledCliques,Circle,0,[],CliqueSegments,[],Lookup),
    findInterconnections(G,ShuffledCliques,Inter),
    segmentsToPositions(Inter,Lookup,InterSegments),
    append(CliqueSegments,InterSegments,Temp),
    transformForGraphiz(G,Temp,Lookup,[],Segments).

/* ============================================================
 * TRANSFORM_FOR_GRAPHIZ
 * G : [
 *      edge(a,b,1.4),
 *      edge(b,c,0.3),
 *      ...
 * ]
 *
 * AND
 * 
 * Segments : [
 *      segment(point(1,2),point(5,10)),
 *      segment(point(5,10),point(11,23)),
 *      ...
 * ] 
 * 
 * AND
 *
 * Lookup: [
 *      (a,point(1,2)),
 *      (b,point(5,10)),
 *      (c,point(11,23)),
 *      ...
 * ]
 * 
 * INTO -->
 * 
 * [
 *     (a,point(1,2),b,point(5,10),1.4),
 *     (b,point(5,10),c,point(11,23),0.3),
 *     ...
 * ]
 *
 * ============================================================ */
transformForGraphiz(_,[],_,Segments,Segments).
transformForGraphiz(G,[Segment|Rest],Lookup,Acc,Segments):-
    segmentPointA(Segment,PointA),
    segmentPointB(Segment,PointB),    
    pointToNode(PointA,Lookup,NodeA),
    pointToNode(PointB,Lookup,NodeB),
    getEdgeWeight(G,NodeA,NodeB,Weight),
    LenseWeight is Weight * Weight,
    append(Acc, [(NodeA,PointA,NodeB,PointB,LenseWeight)],Acc2),
    transformForGraphiz(G,Rest,Lookup,Acc2,Segments).

segmentPointA(segment(P,_),P).
segmentPointB(segment(_,P),P).


getEdgeWeight([edge(A,B,Weight)|_],A,B,Weight).
getEdgeWeight([edge(B,A,Weight)|_],A,B,Weight).
getEdgeWeight([_|Rest],A,B,Weight):-getEdgeWeight(Rest,A,B,Weight).

pointToNode(Point,Lookup,Node) :-
    point(X,Y) = Point,
    [(Other,point(OX,OY))|Rest] = Lookup,
    (
        X == OX, Y == OY ->
            Node = Other
        ;
            pointToNode(Point,Rest,Node)
    ).

/* ============================================================
 * RENDER_GRAPH_HEURISTICS/8
 * ============================================================ */
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
/* BUNCH OF HELPER FUNCTIONS FOR HEURISTIC RENDERING */

calculatePointsForClique(Clique,Circle,X,Y,Result) :-
    length(Clique,N),
    Alpha is 360/N,
    random(0.0,360.0,RandomDeg),
    cliqueRadius(Circle,R),
    calculatePointsForClique(Clique,Alpha,R,X,Y,0,RandomDeg,[],Result)
    .

calculatePointsForClique([],_,_,_,_,_,_,Acc,Acc).
calculatePointsForClique([_|Rest],Alpha,R,X,Y,I,RandomDeg,Acc,Result):-
    Ipp is I + 1,
    AlphaC is Alpha * I,
    addDeg(AlphaC,RandomDeg,RandomAlpha),
    degToPoint(X,Y,RandomAlpha,R,Point),
    append(Acc,[Point],Acc2),
    calculatePointsForClique(Rest,Alpha,R,X,Y,Ipp,RandomDeg,Acc2,Result).

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

/* ============================================================
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
 * ============================================================ */ 
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
    Circle = (Alpha,RB,RSReduced,X,Y).

alpha((A,_,_,_,_),A).
innerRadius((_,R,_,_,_),R).
cliqueRadius((_,_,R,_,_),R).
x((_,_,_,X,_),X).
y((_,_,_,_,Y),Y).
px(point(X,_),X).
py(point(_,Y),Y).

/* ============================================================
 * ARE_NEIGHBORS/3
 * ============================================================ */
areNeighbors(A,B, G) :-
    member(edge(A,B,_), G),!;
    member(edge(B,A,_), G),!.

/* ============================================================
 * GET_NEIGHBORS/3
 *        A = The Node
 *        G = The Graph
 *        N = Result
 * This function has POOR performance!
 * ============================================================ */
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

/* ============================================================
 * FIND_DISTINCT_MAXIMUM_CLIQUES/2
 *		Find all Maximum Cliques in a given Graph G.
 *		Another contraint is that this Cliques cannot have any
 *		Member of the other Cliques, they NEED to be distinct
 *
 *	REMARK regarding performance:
 *		This function implementation is done naive because of time issues.
 *		A better solution would have been if, instead of using the powset
 *		to find all cliques, a more sophisticated algorithm like Bron-Kerbosch
 *		would find all Maximum Cliques.
 *		After calculating all Maximum Cliques the program could filter out those
 *		that share Nodes with each other.  
 * ============================================================ */
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

/* ============================================================
 * FIND_INTERCONNECTIONS/3
 *		Finds all segments that connect all Cliques with each 
 *		other
 * ============================================================ */
findInterconnections(G,MaxCliques,Segments):-
    findInterconnections(G,MaxCliques,[],Segments),!.

findInterconnections(_,[],Segments,Segments).
findInterconnections(G,[Clique|Rest],Acc,Segments):-
    findCliqueInterconnections(G,Clique,Rest,[],CliqueSegments),
    append(Acc,CliqueSegments,Acc2),
    findInterconnections(G,Rest,Acc2,Segments).

findCliqueInterconnections(_,[],_,Segments,Segments).
findCliqueInterconnections(G,[Node|Rest],OtherCliques,Acc,Segments):-
    findNodeInterconnections(G,Node,OtherCliques,[],NodeSegments),
    append(Acc,NodeSegments,Acc2),
    findCliqueInterconnections(G,Rest,OtherCliques,Acc2,Segments).


findNodeInterconnections(_,_,[],Segments,Segments).
findNodeInterconnections(G,Node,[OtherClique|Rest],Acc,Segments):-
    findNodeInterconnectionsInClique(G,Node,OtherClique,[],NodeSegments),
    append(Acc,NodeSegments,Acc2),
    findNodeInterconnections(G,Node,Rest,Acc2,Segments).

findNodeInterconnectionsInClique(_,_,[],Segments,Segments).
findNodeInterconnectionsInClique(G,Node,[Other|Rest],Acc,Segments):-
    (
        areNeighbors(Node,Other,G)->
            append(Acc,[segment(Node,Other)],Acc2)
        ;
            Acc2 = Acc
    ),
    findNodeInterconnectionsInClique(G,Node,Rest,Acc2,Segments).

/* ============================================================
 * GET_NODES/2
 *		Returns a List of all Nodes inside a Graph G
 * ============================================================ */
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

/* ============================================================ *
 *
 *
 *
 *       G E N E R A L  H E L P E R  F U N C T I O N S
 *
 *
 *
 * ============================================================ */
% adds a degree to another one.
% When the value exceeds 360 degree, it goes back again to 0
addDeg(Deg1,Deg2,Result):-
    Temp is Deg1 + Deg2,
    (
        Temp > 360 ->
            Result is Temp - 360
        ;
            Result is Temp
    ).

% converts degree to radians
degToRad(Deg,Rad):-
    Rad is Deg * (pi / 180).

% calculates the euclidean distance
euclideanDistance(point(X1,Y1),point(X2,Y2),Distance):-
    Distance is sqrt(
        (X1-X2)*(X1-X2) +
        (Y1-Y2)*(Y1-Y2)
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

% choose a random element
choose([],[]).
choose(List,Elt):-
    length(List,Length),
    random(0,Length,Index),
    nth0(Index,List,Elt).

% randomly shuffles a list
shuffle([],[]).
shuffle(List,[Element|Rest]):-
    choose(List,Element),
    delete(List,Element,NewList),
    shuffle(NewList,Rest).

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

/*
 * Inserst A into Set only when A is not a member in Set
 */
insertIntoSet(A,Set,Result):-
    (
        member(A,Set) ->
            set(Set,Result)
        ;
            append(Set,[A], Result)
    ).

/*
 * Functions for Accessing
 */
set(A,A).
src(edge(A,_,_),A).
dst(edge(_,B,_),B).

/*
 * Deletes X from the List
 */
delete(X,[X|T], T).
delete(X, [H|T], [H|S]):-
    delete(X,T,S).

/*
 * Checks wheather two lists have equal items
 */
permuation([],[]).
permuation([H|T], R):-
    permutation(T,X), delete(H,R,X).

/*
 * Takes all items from List and removes all Lists
 * from PowSet that have this specific items.
 * The remaining List of Lists will be R
 */
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

min(A,B,Result):-
    (
        A > B ->
            set(B,Result)
        ;
            set(A,Result)
    ).

/* ============================================================ *
 *
 *
 *
 *  		           U N I T  T E S T
 *
 *
 *
 * ============================================================ */

example([
    edge(a,b,0.9),
    edge(a,c,0.3),
    edge(a,e,0.3),
    edge(b,c,1.8),
    edge(c,d,0.8),
    edge(d,e,1.5)
]).

testP1(point(10,0)).
testP2(point(40,0)).

:- begin_tests(social_network).

/* ----------------------------------------- */

test(neighbor) :-
    areNeighbors(a,b,[edge(a,b,0.3), edge(b,c,0.2)]).

/* ----------------------------------------- */

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

/* ----------------------------------------- */

test(isNotClique):-
    example(G),
    not(isClique([a,d],G)),
    not(isClique([a,b,c,e],G)),
    not(isClique([a,b,c,d,e],G)).

/* ----------------------------------------- */

test(isClique):-
    example(G),
    isClique([a,b,c],G),
    isClique([e,d],G).

/* ----------------------------------------- */

test(findInterconnections):-
    example(G),
    distinctCliques(G,Cliques),
    findInterconnections(G,Cliques,Inter),
    Inter == [segment(a,e),segment(c,d)].

/* ----------------------------------------- */

test(transformForGraphiz):-
	transformForGraphiz(
		[	edge(a,b,1),edge(a,c,1),edge(b,c,1)		],
		[	segment(point(10,10),point(0,0)),
			segment(point(10,10),point(5,5)),
			segment(point(0,0),point(5,5))
		],
		[	(a,point(10,10)),(b,point(0,0)),(c,point(5,5))	],
		[],
		Transform
	),
	Transform == [
		(a,point(10,10),b,point(0,0),1),
		(a,point(10,10),c,point(5,5),1),
		(b,point(0,0),c,point(5,5),1)
	].

/* ----------------------------------------- */

test(distance):-
    testP1(P1),
    testP2(P2),
    euclideanDistance(P2,P1,Distance),
    euclideanDistance(P1,P2,Distance2),
    Distance == Distance2,
    Distance == 30.0.

/* ----------------------------------------- */

test(addDeg):-
    addDeg(360,0,R1),
    R1 == 360,
    addDeg(360,1,R2),
    R2 == 1,
    addDeg(20,150,R3),
    R3 == 170,
    addDeg(181,181,R4),
    R4 == 2,
    addDeg(65,350,R5),
    R5 == 55.

/* ----------------------------------------- */

test(soziograph):-
	example(G),
	renderGraph(G,10,10,Seg),
	write(Seg).

/* ----------------------------------------- */

:- end_tests(social_network).