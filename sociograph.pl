:- module(sociograph, [sociograph/1]).
:- use_module(lib/plot_graph, [segments_plot/1]).


sociograph(a).




/* ============== *
 *   UNIT  TEST   *
 * ============== */
:- begin_tests(sociograph).

ex([
    segment(point(1,3),point(6,10)),
    segment(point(3,0),point(6,10))
]).

test(first) :-
    sociograph(a).

test(plot) :-
    ex(Ss),
    segments_plot(Ss).

:- end_tests(sociograph).
