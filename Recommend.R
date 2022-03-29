rm(list = ls())
library(sparklyr)
library(tidyverse)
Dir.Work <- "C:/Users/super/Downloads"
setwd(Dir.Work)

################################################################################
#INIZIALIZZAZIONE DELLE VARIABILI E CARICAMENTO DEL DATASET
################################################################################

N <- 10L
UtRec <- c(2, 924, 8941, 31506)
fileData <- "/dataHW1"

sc <- spark_connect(master = "local")

DataSet <-  spark_read_parquet(sc = sc, 
                               memory = T, # in memory
                               overwrite = T,
                               name = "DataSet",
                               path = paste0(Dir.Work,fileData) 
)



################################################################################
#CREAZIONE DEL DATASET CONTENENTE GLI UTENTI CON AMICI IN COMUNE
################################################################################

Friends_Of_Friends <- inner_join(x = DataSet,
                                 y = DataSet,
                                 by = c("V2"),
                                 suffix = c("", "_CF")
) %>% select(-frAll_CF,-V2) %>% sdf_register(name = "Friends_Of_Friends")



################################################################################
#PULIZIA DEL DATASET PER AVERE SOLO UTENTI CHE NON SIANO GIA' AMICI
################################################################################

Friends_Of_Friends <- Friends_Of_Friends %>%
  filter(!array_contains(frAll,V1_CF)) %>% filter(V1 != V1_CF)



################################################################################
#CONTEGGIO DEGLI AMICI IN COMUNE CHE VERRA' USATO COME RANKING
################################################################################

Friends_Of_Friends <- Friends_Of_Friends  %>% 
  group_by(V1,V1_CF) %>% summarise(n = n()) %>%
  transmute(V1 = V1, V3 = V1_CF, nFriends = n)



################################################################################
#SELEZIONE DEI 10 UTENTI DA SUGGERIRE SECONDO IL LORO RANKING
################################################################################

Recommended <-  Friends_Of_Friends %>% group_by(V1) %>% 
  arrange(-nFriends,V3) %>% 
  mutate(rankFriends = row_number()) %>% 
  filter(rankFriends<=N) %>%
  select(-rankFriends) %>% 
  ungroup()

Solution <- Recommended %>% 
  filter(V1 %in% UtRec)  %>%
  collect() %>%
  arrange(V1,-nFriends,V3)



################################################################################
#STAMPA DELLA SOLUZIONE
################################################################################

Solution %>% group_by(V1) %>% summarise(suggestions = paste(V3, collapse = ","))

spark_disconnect(sc)

