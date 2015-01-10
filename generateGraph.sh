#!/bin/bash
echo generating graph data..
swipl io.pl > output.dot
echo drawing graph..
dot -Kfdp -n -Tpng -o graph.png output.dot
echo done!
