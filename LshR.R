rm(list = ls())
library(mlpack)
library(tidyverse)
Dir.Work <- "C:/Users/super/Downloads"
setwd(Dir.Work)

################################################################################
#CARICAMENTO DEI DATASET
################################################################################

input <- read_csv(file = "lsh.csv.gz", col_names = FALSE)
test <- read_csv(file = "lsh2.csv.gz", col_names = FALSE)

################################################################################
#CREAZIONE DEL MODELLO LSH AND E OR
################################################################################

set.seed(42)

output <- lsh(bucket_size = 100, k = 7, projections = 24,
              query = test, reference = input, tables = 10)

#RIMUOVO L' IMMAGINE STESSA

neigh <- output$neighbors + 1
neigh <- neigh[,-1]

dist <- output$distances
dist <- dist[,-1]

mean_dist <- rowSums(dist) / 6

for (i in 1:20){
  
  zi <- test %>%
        slice(i) %>%
        as.matrix()
  
  xij <- input %>%
          slice(neigh[i,]) %>%
          as.matrix()
  
  for (j in 1:6){
    
    id <- neigh[i,j]
    
    par(mfrow=c(1,2))
    
    image(matrix(zi,ncol = 20,byrow = T), axes=FALSE)
    title(paste0("Immagine test ",i))
    
    image(matrix(xij[j,],ncol = 20,byrow = T), axes=FALSE)
    title(paste0("Immagine più vicina id ", id))
  }
}



