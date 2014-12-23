edge(a,b,0.3).
edge(a,c,0.3).
edge(a,e,0.3).
edge(b,a,0.6).
edge(b,c,0.9).
edge(c,b,0.9).
edge(c,d,0.3).
edge(d,c,0.5).
edge(d,e,0.7).
edge(e,d,0.8).

hasDirectEdge(A,B):- edge(A,B,_),!.*/
