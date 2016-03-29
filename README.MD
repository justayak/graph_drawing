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
