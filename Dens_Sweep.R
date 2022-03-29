rm(list = ls())

library(igraph)
library(Matrix)
library(tidyverse)
library(sparklyr)

################################################################################
# INCLUSIONE DEL FILE SORGENTE PER LE FUNZIONI E APERTURA DI SPARK
################################################################################

Dir.Work <- "C:/Users/super/Desktop/Homework 2/Codici"
setwd(Dir.Work)

source("./dense_subGraph.R")

sc <- spark_connect(master = "local")



################################################################################
# CARICAMENTO DEI DATI DEL GRAFO
################################################################################

list_files_all <- dir(path = paste0(Dir.Work,"/dataHW1"),  
                      pattern = ".parquet",
                      full.names = TRUE)

DataSet <-  spark_read_parquet(sc = sc, 
                               memory = T,
                               overwrite = T,
                               name = "friends",
                               path = list_files_all 
)



################################################################################
# ESTRAZIONE DEI NODI E DEGLI ARCHI E COLLECT IN R
################################################################################

vertR <- DataSet %>%
  transmute(id = V1) %>% 
  sdf_drop_duplicates() %>% collect()

edgR <- DataSet %>% 
  transmute(src = V1, dst = V2) %>% 
  sdf_drop_duplicates() %>% collect()

spark_disconnect(sc = sc)



################################################################################
# CALCOLO DELLA COMUNITA' DENSA E STAMPA DELLE SUE CARATTERISTICHE
################################################################################

Subgraph <- findDenseSub(vertices = vertR, edges = edgR)

n_vert <- gorder(Subgraph)
paste("Number of vertices: ", n_vert)

n_edges <- gsize(Subgraph)
paste("Number of edges: ", n_edges)

rho <- densGr(Subgraph)
paste("Density: ", rho)



################################################################################
# CREAZIONE DEL GRAFO E DELLE COMPONENTI CONNESSE
################################################################################

G <- igraph::simplify(graph_from_data_frame(d = edgR,
                                                directed=FALSE,
                                                vertices = vertR),
                          remove.multiple = TRUE, remove.loops = TRUE)

G <- components(G)



################################################################################
# CREAZIONE DEL SOTTOGRAFO COMPOSTO DALLA SOLA COMPONENTE CONNESSA
# CONTENENTE IL NODO DI ID 1
################################################################################

x <- as.numeric(G$membership[which(names(G$membership) == "1")])

vert <- as.numeric(names(which(G[["membership"]] == x)))

edges <- edgR[which(edgR$dst %in% vert),]

Graph <- graph_from_data_frame(d = edges,
                               directed=FALSE,
                               vertices = vert)



################################################################################
# INIZIALIZZAZIONE VETTORE DI TELETRASPORTO E USO DI PAGERANK
################################################################################

Restart = rep(x = 0, length.out = length(vert))
Restart[which(vert == 1)] <- 1

PageRank <- page_rank(graph = Graph,
                      damping = 0.85,
                      personalized = Restart)



################################################################################
# CREAZIONE DEL GRAFO PERMUTATO
################################################################################

Order_PageRank <- sort(x = PageRank$vector,
                       decreasing = TRUE, 
                       index.return = TRUE)$ix

Permuted_Graph <- permute(Graph, invPerm(Order_PageRank))



################################################################################
# CALCOLO DELLA MATRICE DI ADIACENZA E DEI DATI PER LA CONDUTTANZA
################################################################################

Adj <- as_adjacency_matrix(Permuted_Graph, 
                           type = c("both"))

D <- rowSums(Adj)
which((D - degree(Permuted_Graph)) != 0)
Vol_Adj <- cumsum(D)
Cut_Adj <- cumsum(D) - 2*cumsum(rowSums(tril(Adj)))



################################################################################
# GRAFICO DELLA CONDUTTANZA
################################################################################

ggplot(data = tibble(x = Vol_Adj,y = Cut_Adj) %>%
         mutate(n = row_number()),aes(x=n,y = y/x)) +
  geom_line() +
  labs(x = "Node rank i in decreasing PPR score",
       y = "Conductance") +
  theme_classic()


