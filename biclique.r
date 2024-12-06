## Lehmann, Schwartz, and Hansen (2008)
## For use with igraph objects
## via http://stackoverflow.com/questions/31024481/detect-bi-cliques-in-r-for-bipartite-graph

library(igraph)

biclique <- function(graph, k, l) {
   
   vMode1 <- c()
   if (!is.null(V(graph)$type)) {
      
      vMode1 <- which(!V(graph)$type)
      vMode1 <- intersect(vMode1, which(degree(graph) >= l))
   }
   
   nb <- as_adj_list(graph)
   
   bicliques <- list()
   
   if (length(vMode1) >= k) {
      
      comb <- combn(vMode1, k)
      i <- 1
      sapply(1:ncol(comb), function(c) {
         
         commonNeighbours <- c()
         isFirst <- TRUE
         
         sapply(comb[,c], function(n) {
            
            if (isFirst) {
               
               isFirst <<- FALSE
               commonNeighbours <<- nb[[n]]
            } else {
               
               commonNeighbours <<- intersect(commonNeighbours, nb[[n]])
            }
         })
         
         if (length(commonNeighbours) >= l) {
            
            bicliques[[i]] <<- list(m1=comb[,c], m2=commonNeighbours)
         }
         
         i <<- i + 1
      })
   }
   bicliques <- bicliques[!sapply(bicliques,is.null)]
   return(bicliques)
}