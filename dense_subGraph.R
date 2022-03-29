# dens(S) = (#edges in S)/(#vertices in S)
densGr <- function(Gr){
  # (#vertices in S)
  nRows <- gorder(Gr)
  # handle 0/0 = 0
  if(nRows > 0){
    return(gsize(Gr) / nRows)
  } else {
    return(0)
  }
}



findDenseSub <- function(vertices, edges, epsilon = 0.1){
  countPass <- 0L
  # S,ST <- V
  GraphS <- GraphST <- igraph::simplify(graph_from_data_frame(d = edges,
                                                              directed=FALSE,
                                                              vertices = vertices),
                                        remove.multiple = TRUE, remove.loops = TRUE)
  

                      
  
  STnrow = gorder(GraphST)
  
  # while ST in not empty do
  while(STnrow > 0){
    cat(paste0("Round n. ",  countPass <- countPass + 1L, ", num. vertices = ",STnrow,"\n"))
    
    # A(ST) := {i in ST | deg_ST(i) <= 2(1 + eps) dens(ST)} 
    # ST <- ST \ A(ST) =  {i in ST | deg_ST(i) > 2(1 + eps) dens(ST)}
    
    densS2 <- densGr(GraphST)
    verticesSelection <- degree(GraphST) > 2*(1+epsilon)*densS2 
    GraphST <- induced_subgraph(GraphST, which(verticesSelection))
    
    STnrow = gorder(GraphST)
    
    # if dens(ST) > dens(S) then 
    if(densGr(GraphST)>densGr(GraphS)){
      # S <- ST
      GraphS <- GraphST
    # end if
    }
  # end while
  }
  cat(paste0("Ended!\n"))
  # return S
  return(GraphS)
}
