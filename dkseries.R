dkseries <- function(data, ego_col, alter_col, iter_mult, dk) {
  # Extract vectors for the ego (origin) and alter (destination) columns
  v1 <- data[[ego_col]]
  v2 <- data[[alter_col]]
  
  N <- length(v1)
  w <- iter_mult * N  # Total number of iterations
  
  # Extract unique origin nodes, mimicking Stata's `levelsof i`
  nodes <- unique(v1)
  nlevs <- length(nodes)
  
  z <- 1 # Initialize counter
  
  if (dk == 0) {
    # ---------------------------
    # dk = 0: 0K-null model swap
    # ---------------------------
    while (z <= w) {
      skip <- FALSE
      
      # Select a random edge (equivalent to 1+trunc(N*runiform(1,1)) in Mata)
      e1 <- sample(1:N, 1)
      k <- v1[e1]
      l <- v2[e1]
      
      # Select two random nodes from the unique node list
      m <- nodes[sample(1:nlevs, 1)]
      n <- nodes[sample(1:nlevs, 1)]
      
      # Check that randomly selected vertices are not the same as the swapped edge
      if (k == m || k == n || l == m || l == n || m == n) {
        skip <- TRUE
      }
      
      if (!skip) {
        # Vectorized degree count for k and l
        kk <- sum(v1 == k)
        kl <- sum(v1 == l)
        
        # Check if the m -> n edge already exists
        if (any(v1 == m & v2 == n)) {
          skip <- TRUE
        }
        
        # Skip if deletion makes either k or l an isolate
        if (kl == 1 || kk == 1) {
          skip <- TRUE
        }
      }
      
      if (!skip) {
        # Create new swapped edge m -> n and replace the old one
        v1[e1] <- m
        v2[e1] <- n
        z <- z + 1
      }
    }
    
  } else if (dk == 1) {
    # ---------------------------
    # dk = 1: 1K-null model swap
    # ---------------------------
    while (z <= w) {
      skip <- FALSE
      e1 <- sample(1:N, 1)
      e2 <- sample(1:N, 1)
      
      while (e1 == e2) {
        e1 <- sample(1:N, 1)
        e2 <- sample(1:N, 1)
      }
      
      k <- v1[e1]
      l <- v2[e1]
      m <- v1[e2]
      n <- v2[e2]
      
      if (k == m || k == n || l == m || l == n) {
        skip <- TRUE
      }
      
      if (!skip) {
        # Vectorized check to see if the proposed edges k->n or m->l already exist
        if (any((v1 == k & v2 == n) | (v1 == m & v2 == l))) {
          skip <- TRUE
        }
      }
      
      if (!skip) {
        # Swap targets
        v2[e1] <- n
        v2[e2] <- l
        z <- z + 1
      }
    }
    
  } else if (dk == 2) {
    # ---------------------------
    # dk = 2: 2K-null model swap
    # ---------------------------
    while (z <= w) {
      skip <- FALSE
      e1 <- sample(1:N, 1)
      e2 <- sample(1:N, 1)
      
      while (e1 == e2) {
        e1 <- sample(1:N, 1)
        e2 <- sample(1:N, 1)
      }
      
      k <- v1[e1]
      l <- v2[e1]
      m <- v1[e2]
      n <- v2[e2]
      
      if (k == m || k == n || l == m || l == n) {
        skip <- TRUE
      }
      
      if (!skip) {
        # Vectorized degree count for origin nodes k and m
        kk <- sum(v1 == k)
        km <- sum(v1 == m)
        
        # Check if reshuffled links already exist
        if (any((v1 == k & v2 == n) | (v1 == m & v2 == l))) {
          skip <- TRUE
        }
        
        # Check that origin vertices in each edge have the same degree
        if (kk != km) {
          skip <- TRUE
        }
      }
      
      if (!skip) {
        # Swap targets
        v2[e1] <- n
        v2[e2] <- l
        z <- z + 1
      }
    }
  } else {
    stop("Error: dk must be 0, 1, or 2.")
  }
  
  # Update the dataframe and return it
  data[[ego_col]] <- v1
  data[[alter_col]] <- v2
  return(data)
}