rm(list = ls())
library(igraph)
library(Matrix)
library(tidyverse)

Dir.Work <- "C:/Users/super/Desktop/Homework 2/Codici"
setwd(Dir.Work)



################################################################################
# CARICAMENTO DEI DATI E COSTRUZIONE DELLE MATRICI BASE
################################################################################

load("UserShows.RDATA")

R = Matrix(UserShows, sparse = TRUE)

Past <- Diagonal(x = rowSums(R) ^ (-0.5))
Qast <- Diagonal(x = colSums(R) ^ (-0.5))



################################################################################
# COSTRUZIONE DEL MODELLO PREDITTIVO USER-USER ED ESTRAZIONE DEI PRIMI 100 SHOW
################################################################################

Gamma_Users <- Past %*%  tcrossprod(R,R) %*% Past %*% R

Suggested_Claudia_Users <- Gamma_Users[500,1:100]

Suggested_Claudia_Users <- sort(Suggested_Claudia_Users, 
                                decreasing = TRUE, 
                                index.return = T)$ix



################################################################################
# STAMPA DEI PRIMI 5 SHOW DA CONSIGLIARE USER-USER
################################################################################

cat(paste(as.vector(Shows$name[Suggested_Claudia_Users[1:5]]),collapse = "\n"))



################################################################################
# COSTRUZIONE DEL MODELLO PREDITTIVO ITEM-ITEM ED ESTRAZIONE DEI PRIMI 100 SHOW
################################################################################

Gamma_Items <- R %*% Qast %*% crossprod(R,R) %*% Qast

Suggested_Claudia_Items <- Gamma_Items[500,1:100]

Suggested_Claudia_Items <- sort(Suggested_Claudia_Items, 
                                decreasing = TRUE, 
                                index.return = T)$ix



################################################################################
# STAMPA DEI PRIMI 5 SHOW DA CONSIGLIARE ITEM-ITEM
################################################################################

cat(paste(as.vector(Shows$name[Suggested_Claudia_Items[1:5]]),collapse = "\n"))



################################################################################
# COSTRUZIONE DEL GRAFO
################################################################################

graph <- graph.incidence(R)



################################################################################
# PAGERANK CON RIPARTENZA ED ESTRAZIONE DEI PRIMI 100 SHOW
################################################################################

Restart <- rep_len(0,10485)
Restart[500] <- 1

PageRankClaudia <- page_rank(graph = graph, personalized = Restart)

Suggested_Claudia_Pagerank <- PageRankClaudia$vector[9986:10085]

Suggested_Claudia_Pagerank <- sort(Suggested_Claudia_Pagerank, 
                                    decreasing = TRUE, 
                                    index.return = T)$ix 



################################################################################
# STAMPA DEI PRIMI 5 SHOW DA CONSIGLIARE PAGERANK
################################################################################

cat(paste(as.vector(Shows$name[Suggested_Claudia_Pagerank[1:5]]),collapse = "\n"))



################################################################################
# CALCOLO DELLE PERFORMANCE
################################################################################ 

Performance <- bind_rows(
  tibble(y = cumsum(Claudia[Suggested_Claudia_Users])/ seq_len(100)) %>% 
    mutate(x = row_number(), z = "user-user") ,
  tibble(y = cumsum(Claudia[Suggested_Claudia_Items])/ seq_len(100)) %>% 
    mutate(x = row_number(), z = "item-item") ,
  tibble(y = cumsum(Claudia[Suggested_Claudia_Pagerank])/ seq_len(100)) %>%
    mutate(x = row_number(), z = "page-rank") )



################################################################################
# PLOT PER CONFRONTARE LE PERFORMANCE
################################################################################

ggplot(data = Performance %>% filter(x<51)) +
  geom_line(aes(y = y, x = x, color = z), size = 1.5) + labs(title = "Confronto delle performance",
                                               x = "Numero di suggerimenti",
                                               y = "Frazione di suggerimenti corretti") + 
  theme_classic()
