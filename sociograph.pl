:- module(sociograph, [
    sociograph/1,
    min/3
]).
:- use_module(lib/plot_graph, [segments_plot/1]).
:- use_module(graph_utils, [distinctCliques/2, example/1]).

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
    defineCircleParameters(DistinctMaxCliques,W,H,Circle)
.

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
    Alpha is 360/N,
    RB is S / (2 * (1 + sin(Alpha/2))),
    RS is RB * sin(Alpha/2),
    RSReduced is RS * 0.9,
    Circle = (Alpha,RB,RSReduced)
    .
    

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


/* ============== *
 *   UNIT  TEST   *
 * ============== */
:- begin_tests(sociograph).

ex([
    segment(point(1,3),point(6,10)),
    segment(point(3,0),point(6,10))
]).

test(first) :-
    graph_utils:example(G),
    graph_utils:distinctCliques(G,C),
    defineCircleParameters(C,800,600,Ci),
    writeln(Ci).

test(plot) :-
    ex(Ss),
    segments_plot(Ss).

:- end_tests(sociograph).
