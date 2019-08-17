
msTreeKruskal <- function(nodes, arcs) {
  
  # Order arcs by weight
  arcs <- matrix(arcs[order(arcs[, 3]), ], ncol = 3)
  
  # Components
  components <- matrix(c(nodes, nodes), ncol = 2)
  
  # Initialize tree with first arc
  tree.arcs <- matrix(ncol = 3)[-1, ]
  
  stages <- 0  # initialize counter
  stages.arcs <- c()  # vector to store stage number in wich each arc was added
  
  # Start with first arc
  i <- 1
  # Repeat until we have |N|-1 arcs
  while(nrow(tree.arcs) < length(nodes) - 1) {
    
    # Select arc
    min.arc <- arcs[i, ]
    
    # Check components of the two nodes of selected arc
    iComp <- components[components[, 1] == min.arc[1], 2]
    jComp <- components[components[, 1] == min.arc[2], 2]
    if (iComp != jComp) {
      # Add arc to msTree
      tree.arcs <- rbind(tree.arcs, min.arc)
      # Merge components
      components[components[, 2] == jComp, 2] <- iComp
    }
    
    stages <- stages + 1  # counter
    # Save in which stage an arc was added to the tree and update
    stages.arcs <- c(stages.arcs,
                     rep(stages, nrow(tree.arcs) - length(stages.arcs)))
    # Continue with next arc
    i <- i + 1
    
  }
  
  # Column names
  colnames(tree.arcs) <- c("ept1", "ept2", "weight")
  # Remove row names
  rownames(tree.arcs) <- NULL
  
  output <- list("tree.nodes" = nodes, "tree.arcs" = tree.arcs,
                 "stages" = stages, "stages.arcs" = stages.arcs)
  return(output)
  
}