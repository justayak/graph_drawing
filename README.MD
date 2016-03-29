#Graph Drawing

(University mini project: Polytech Nantes - Knowledge-based systems)

## Description
Given an application that simulates the behavior of a crowd of people on a process. Every person has a unique set of friends that he trusts individually (determined through a number) that does not necessarily correlate with the value of the friend. This friendship relation can be represented as a weighted, directed graph where the vertices are persons and the weighted, directed edges are the esteem that a certain person has for the destination vertice. The higher the value, the higher the trust.

![screenshot_20160329_130157](https://cloud.githubusercontent.com/assets/831215/14105859/73e1bdfc-f5ae-11e5-9d05-6a7268cea18d.png)

This results in a complex social network graph which has the following settings:

![](https://cloud.githubusercontent.com/assets/831215/14106225/1bed3a52-f5b0-11e5-885a-ec19c22d6840.png)

We define the Graph as follows:

![](https://cloud.githubusercontent.com/assets/831215/14106240/3fba5f46-f5b0-11e5-9502-958cfe0b4c2c.png)

We want the resulted graphic to:

![](https://cloud.githubusercontent.com/assets/831215/14106233/2bc566e8-f5b0-11e5-8d9c-b5162de169b5.png)

## General Solution
### Assumptions
Nodes have a relatively low number of close friends. We also assume that friendships are rather clustered, that means, that friends of a node are also likely to be friends with each other (transitivity) - we will call this group a clique. Further, we assume that cliques might overlap, which means that certain people are members in multiple cliques. We also assume that the social network is a small-world, meaning that the diameter of the graph is relatively small.

### Precise description of an example

![](https://cloud.githubusercontent.com/assets/831215/14105876/836b8654-f5ae-11e5-8b21-7ad8f00a3ccc.png)

Example Graph G:

![](https://cloud.githubusercontent.com/assets/831215/14105909/a7d8740c-f5ae-11e5-8f06-4800ed9f1ee2.png)

### Basic Shape
Circle: Used by Jacob L. Moreno, a psychiatrist, to draw social network graphs when he had no specific basis for arranging points in one particular form or another.

![](https://cloud.githubusercontent.com/assets/831215/14105915/abee87ca-f5ae-11e5-98b1-c3c0ac041b9d.png)

![nbr1](https://cloud.githubusercontent.com/assets/831215/14106389/1ff6e886-f5b1-11e5-9427-0568e0267686.png)
![nbr2](https://cloud.githubusercontent.com/assets/831215/14106399/2d273c22-f5b1-11e5-9719-19f809c424de.png)
![nbr3](https://cloud.githubusercontent.com/assets/831215/14106408/3903919e-f5b1-11e5-8c5e-e0003f96f5cd.png)
![nbr4](https://cloud.githubusercontent.com/assets/831215/14106410/435270a2-f5b1-11e5-9062-bbf0b99649a8.png)
![nbr5](https://cloud.githubusercontent.com/assets/831215/14106422/512e1b54-f5b1-11e5-904d-f35e477c75e5.png)
![nbr6](https://cloud.githubusercontent.com/assets/831215/14106433/5fbbd9cc-f5b1-11e5-917b-b10c89ac8890.png)
![nbr7](https://cloud.githubusercontent.com/assets/831215/14106440/6da4ff32-f5b1-11e5-98be-30d108fb1059.png)

With our approach we cannot satisfy all of our requirements but rather need to compromise with others: 

![](https://cloud.githubusercontent.com/assets/831215/14106511/dcb42696-f5b1-11e5-9de3-c33fa0d95e77.png)

## Actual Solution

### Preprocessing

The first step to draw the graph is to find all maximum cliques. To do so, we will use the Bron-Kerbosch algorithm. To do so we must transform the directed, weighted graph into an undirected graph. To do so, we must simplify our graph. Two edges will be transformed as following:

![](https://cloud.githubusercontent.com/assets/831215/14105947/d840a51a-f5ae-11e5-8caa-1e3fbdfe058b.png)

In the directed graph, the edges are represented by a 3-Tuple, the source node is the first element, than the destination node and finally the weight of the edge. When transformed into the undirected graph, an edge is represented as a 4-Tuple: (Source, Destination, Sum of the weights of both edges between two nodes, Boolean that tells, of the edge is bidirectional (TRU) or unidirectional (FAL)). If an edge is ultimatly unidirectional (in the previous, directional graph), the direction can be restored by the order of the Nodes (first two elements of the Tuple). However, the transformation for nodes that are interconnected (bidirectional) is destructive (!) and cannot be fully restored. In the end, this is not important, as we assume that most relationships are bidirectional and are of similar strength.

### Find all Distinct Maximum Cliques
![](https://cloud.githubusercontent.com/assets/831215/14106560/3486d3e6-f5b2-11e5-9e6e-6ae9661f1fb9.png)

![alg1](https://cloud.githubusercontent.com/assets/831215/14105956/ebe54378-f5ae-11e5-8eea-17f10efe37e2.png)

![algexp](https://cloud.githubusercontent.com/assets/831215/14106570/57abba8a-f5b2-11e5-8672-de44b3831f7d.png)

![alg1b](https://cloud.githubusercontent.com/assets/831215/14105969/f7b2322e-f5ae-11e5-9cc4-6e32b338fa5a.png)

When using Bron-Kerbosch for generating all Candidates instead of the powerset, the number of Candidates is greatly reduced.

### Define the Circles
As we want to use circles to draw the graph we now need to find the different radius and positions for the sub circles. There are two main approaches to apply the sub circles:
* (1) The circles size depends on the size of the cliques thus making big cliques take more space and small cliques take less.
* (2) Use the same radius for all circles independently from its cliques actual size. 

![](https://cloud.githubusercontent.com/assets/831215/14105990/14ceebcc-f5af-11e5-988d-381f43b1f364.png)

We will use the second approach where all sub circles have the same size. The reasons for this are:
* **Simplicity**: Calculating the circles and their radius is easier with fixed size circles and it is easier to predict
* **Appeal to Domain**: As suggested by sociology, human cliques are usually of similar size thus making it more convenient when the circles have the same size, as this
* **makes clusters more visible**: as the visual density significantly rises.

#### Calculating the circles parameters

![](https://cloud.githubusercontent.com/assets/831215/14106028/3237922c-f5af-11e5-80c3-40b3f1cad392.png)

The following function deducts the basic parameters for the circles (with the assumption that the big circle will be drawn in the pictures center):

![agl2](https://cloud.githubusercontent.com/assets/831215/14106045/40bd67ae-f5af-11e5-9ab0-0611a33dd33f.png)

With this parameters we can now draw a basic graphic:

![alg3](https://cloud.githubusercontent.com/assets/831215/14106066/52428de2-f5af-11e5-9f6a-92918657fd42.png)

![alg4](https://cloud.githubusercontent.com/assets/831215/14106066/52428de2-f5af-11e5-9f6a-92918657fd42.png)

The following function is the most primitive way of drawing the Cliques circle as no heuristic to position its nodes is used whatsoever.

![alg5](https://cloud.githubusercontent.com/assets/831215/14106085/6933d380-f5af-11e5-8fb6-365efd6ab1e7.png)

This algorithms provide us with a very basic first graph:

![](https://cloud.githubusercontent.com/assets/831215/14106098/75dac4b8-f5af-11e5-823a-92416c506308.png)

However, there are three main problems with this graph:
* (1) The weight of the friendship is not shown yet
* (2) There are unnecessary intersections because nodes with many connections to other cliques are not put in optimal, central places
* (3) The image looks boring, as the graphic is very static (many similar angles)

Issue 1 can be addressed by simply adapting the rendering function (provided in: plot_graph:segment_plot/1). Issue 2 can be addressed by implementing a heuristic to put every node in its best position inside the clique, more precisely: a node with many links to other cliques should face inwards while a node with few or none links to other cliques should face outwards. This will reduce the number of crossings through the clique. Issue 3 can simply be addressed by adding a random angle to each clique that is rendered, making the visual appearance more diverse and interesting.

To fix the 2. issue, we must adapt the calculateCliqueCircle function (algorithm 4 and algorithm 5).

![](https://cloud.githubusercontent.com/assets/831215/14106714/70109fcc-f5b3-11e5-9856-b768d2b5e5a7.png)

![alg6](https://cloud.githubusercontent.com/assets/831215/14106126/920aa234-f5af-11e5-8127-c37733b055e5.png)

When this algorithm is applied, following graphic will be rendered:

![](https://cloud.githubusercontent.com/assets/831215/14106136/a4ab5f3c-f5af-11e5-8d7c-9435b85bb701.png)

Applied to a more complex graph: 

![](https://cloud.githubusercontent.com/assets/831215/14106150/b71c525c-f5af-11e5-9008-1f16ee839a91.png)
