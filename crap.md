



 (e.g., the proportion of shortest paths between the young man and the pedestrian that include Vincent as an intermediary). 

First we need to figure out the denominator of @eq-pair (the number of shortest paths between the young man and the pedestrian). We can do this using the `igraph` function `all_shortest_paths`:




So the denominator for equation @eq-pair is the length of the `ap$vpaths` list object (the number of elements in the list):


```{r}
    den <- length(ap$vpaths)
```

The numerator of @eq-pair is the subset of these paths containing Vincent. We can obtain those using the `sapply` function in `R` applied to the list of shortest paths:


```{r}
    num <- sum(sapply(ap$vpaths, function(x) {"Vincent" %in% names(x)}))
    num
```

The function argument just checks whether the character string "Vincent" is present in the named vector of node labels in each path stored in `vpaths`. The sum of the (one/zero or logical TRUE/FALSE) entries of this vector is the number of times Vincent is in a path linking The Pedestrian and The Young Man. 

And the pair dependency of the young man and the pedestrian on Vincent is just:

```{r}
    pd <- num/den
    round(pd, 3)
```



As noted in @eq-bet1, Vincent's *betweenness* is just the sum of the dependencies of each pair of nodes in the graph on him. We could compute that (not very efficiently) as follows:


```{r}
    bet.vincent <- 0
    v <- which(V(g)$name == "Vincent")
    node.vec <- c(1:vcount(g))[-v]
    for (i in node.vec) {
        for (j in node.vec) {
            if (i < j & are_adjacent(g, i, j) == FALSE) {
                p <- all_shortest_paths(g, from = i, to = j)$vpaths #all shortest paths from i to j
                bet.vincent <- bet.vincent + sum(sapply(p, function(x) {v %in% x}))/length(p) #paths that involve Vincent divided by total
                }
            }
        }
```

The first lines initializes Vincent's betweenness at zero. Then we store the position of Vincent in the node id vector in line 2 using the base `R` function `which`, and then create a vector of node ids in line 3 that excludes Vincent. We then loop through all the remaining node ids (making sure we only deal with starting and end nodes that are not adjacent) in lines 4-11, adding the dependencies of each considered pair on Vincent to his running score in line 8. 

The result is:

```{r}
    bet.vincent
```

Which is the same result we would have gotten had we used the `betweenness` function in `igraph` specifying that we want the answer just for the Vincent node:


```{r}
    betweenness(g, v = "Vincent")
```

We can also tweak the above code to write a function that computes the one-sided dependency of one node on any other node:

```{r}
    one.sided <- function(w, a, b) {
        delta.ik <- 0 #initializing one-sided dependence score
        v.i <- which(V(w)$name == a)
        v.k <- which(V(w)$name == b)
        for (v.j in 1:vcount(w)) {
            if (are_adjacent(w, v.i, v.j) == FALSE & v.i != v.j & v.j != v.k & v.i != v.k) {
                p <- all_shortest_paths(w, from = v.i, to = v.j)$vpaths #all shortest paths from i to j
                delta.ik <- delta.ik + sum(sapply(p, function(x) {v.k %in% x}))/length(p) #paths that involve k divided by total
                }
            }
        return(delta.ik)
        }
```

The function takes the graph (`w`), and the names of the dependent (`a`) and the intermediary nodes (`b`) as arguments and returns a one-sided dependence score `delta.ik`. The loop just goes once through graph, checking to see if the destination node is a neighbor of the dependent node. 

So if we wanted to find out the one-sided dependence of the Pedestrian on Vincent, we could just type:

```{r}
    one.sided(g, "Pedestrian", "Vincent")
```

Which is the sum of the pair dependencies that involve the Pedestrian as a starting node and Vincent as an inner node.

Note that while the Pedestrian is highly dependent on Vincent to reach others, the reverse is not the case:

```{r}
    one.sided(g, "Vincent", "Pedestrian")
```

As the Pedestrian stands on none of the paths that link Vincent to other nodes in the graph. 

We can of course use the same `igraph` function to---efficiently, using Brandes's [-@brandes01] algorithm---compute the betweenness centrality of each node in the graph stored in a vector of length $N$:


```{r}
    pulp.bet <- betweenness(g)
```

We should expect a character to have high betweenness in this network to the extent that they appear in scenes with characters who themselves don't appear in any scenes together, thus inter-mediating between different parts of the story. Characters who only appear in one scene with some others (like The Wolf or The Gimp) are likely to be low in betweenness.

Using the information stored in the `pulp.bet` vector of betweenness centralities for each node, we can create a top ten table of betweenness for the *Pulp Fiction* network.

```{r}
    library(kableExtra)
    top.10.bet <- sort(pulp.bet, decreasing = TRUE)[1:10]
    kbl(round(top.10.bet, 2), format = "pipe", align = c("l", "c"),
        col.names = c("Character", "Betweenness"),
        caption = "Top Five Betweenness Characters in Pulp Fiction Network.") |> 
    kable_styling(bootstrap_options = c("hover", "condensed", "responsive"))
```

Unsurprisingly, the three main characters in the story are also the highest in betweenness, with the already considered Vincent at the top of the list (that makes sense since Vincent intermediates between Butch and the rest of the story as he sadly found out in the [toilet](https://www.youtube.com/watch?v=fm9VPN1r4Hk)). Somewhat surprisingly, the main antagonist of the story (the pawn shop owner) is also up there. After that we see a big drop in the bottom five of the top ten. 
