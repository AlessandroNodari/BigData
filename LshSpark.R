rm(list = ls())
library(sparklyr)
library(tidyverse)
Dir.Work <- "C:/Users/super/Downloads"
setwd(Dir.Work)

sc <- spark_connect(master = "local")

################################################################################
#CARICAMENTO DEL DATASET DELLE IMMAGINI E CREAZIONE VERSIONE ASSEMBLED
################################################################################

typeCols <- paste0("V",1:400,"= 'double'",collapse = ",")
typeCols <- paste("list(",typeCols,")")

datasetImages <-  spark_read_csv(sc, name = "datasetImages",
                                 path = "lsh.csv.gz",
                                 columns = eval(parse(text = typeCols)),
                                 header = FALSE, memory = F)

DatasetImagesR <- read_csv(file = "lsh.csv.gz",
                        col_names = FALSE)

datasetImages_Assembled <- datasetImages %>% 
  ft_vector_assembler(input_cols = paste0("V",1:400),output_col = "vect") %>%
  select(vect) %>% 
  sdf_with_unique_id %>% 
  mutate(id = int(id)) %>%
  sdf_register(name = "datasetImages_Assembled")

tbl_cache(sc, "datasetImages_Assembled")



################################################################################
#CARICAMENTO DEL DATASET DELLE IMMAGINI TEST E CREAZIONE VERSIONE ASSEMBLED
################################################################################

ImagesTestR <- read_csv(file = "lsh2.csv.gz",
                        col_names = FALSE)

names(ImagesTestR) <- paste0("V",1:400)

tbfImages <-  copy_to(dest = sc,
                      df = ImagesTestR,
                      name = "tbfImages",
                      memory = F)

tbfImages_Assembled <- tbfImages %>% 
  ft_vector_assembler(input_cols = paste0("V",1:400),output_col = "vect") %>%
  select(vect) %>% 
  sdf_with_unique_id %>% 
  mutate(id = int(id)) %>%
  sdf_register(name = "tbfImages_Assembled")

tbl_cache(sc, "tbfImages_Assembled")



################################################################################
#CALCOLO DEL MODELLO LSH E FITTING SUI DATI
################################################################################

L <- 10L
k <- 24L
r <- 100L

model_LSHo <- ft_bucketed_random_projection_lsh(sc,
                                                input_col = "vect",
                                                output_col = "buckets",
                                                bucket_length = r,
                                                num_hash_tables = (L*k),
                                                seed = 42)

model_LSHfitted <- ml_fit(model_LSHo,datasetImages_Assembled)



################################################################################
#CALCOLO DEGLI HASH PER LE IMMAGINI DA CUI ESTRARRE I VICINI
################################################################################

hash1 <- ml_transform(model_LSHfitted,datasetImages_Assembled) %>% 
  select(-vect) %>%
  ft_sql_transformer("SELECT id,POSEXPLODE(buckets) AS (pos, valueInt) FROM __THIS__") %>%
  sdf_separate_column(column = "valueInt", into = "value") %>%
  mutate(nBand = as.integer(pos / k)) %>%
  select(-valueInt,-pos) %>%
  group_by(id,nBand) %>% summarise(bucket = hash(collect_list(value)))



################################################################################
#CALCOLO DEGLI HASH PER LE IMMAGINI TEST
################################################################################

model_LSHfitted2 <- ml_fit(model_LSHo,tbfImages_Assembled)

hash2 <- ml_transform(model_LSHfitted2,tbfImages_Assembled) %>% 
  select(-vect) %>%
  ft_sql_transformer("SELECT id,POSEXPLODE(buckets) AS (pos, valueInt) FROM __THIS__") %>%
  sdf_separate_column(column = "valueInt", into = "value") %>%
  mutate(nBand = as.integer(pos / k)) %>%
  select(-valueInt,-pos) %>%
  group_by(id,nBand) %>% summarise(bucket = hash(collect_list(value)))



################################################################################
#CALCOLO, CON UN JOIN, DEI CANDIDATI VICINI E PULIZIA DEGLI STESSI
################################################################################

Candidates <- inner_join(x=hash1,
                         y=hash2,
                         by = c("nBand","bucket"),
                         suffix = c("_A", "_B")
)

Candidates <- Candidates %>%
  sdf_register(name = "Candidates") %>% 
  select(-nBand,-bucket)

Cand <- Candidates %>% collect()
Cand <- unique(Cand)



################################################################################
#CALCOLO DELLE SEI (SE CE NE SONO) IMMAGINI PIU' VICINE
################################################################################

distanze <- rep(0,20)

for (i in 0:19){
  
  vettore <- Cand %>% filter(id_B == i)
  vettore <- vettore[,1] + 1
  vettore <- t(vettore)
  
  zi <- ImagesTestR%>%
        slice(i+1) %>%
        as.matrix()
  
  xij <- DatasetImagesR %>%
          slice(vettore) %>%
          as.matrix()
  
  distanceL2 <- sqrt(rowSums((xij - matrix(zi, ncol = 400, nrow = ncol(vettore), byrow = T))^2))
  z <- 1:length(distanceL2)
  
  count <- 0
  somma <- 0
  
  ##############################################################################
  #STAMPA DELLE IMMAGINI
  ##############################################################################
  
  while (count < 6){
    
    if (length(distanceL2) == 0){
      break
    }
    
    id <- z[which(distanceL2 == min(distanceL2))]
    
    for (h in 1:length(id)){
      
      xiast <- xij[id[h],]
      
      par(mfrow=c(1,2))
      
      image(matrix(zi,ncol = 20,byrow = T), axes=FALSE)
      title(paste0("Immagine test ",i+1))
      
      image(matrix(xiast,ncol = 20,byrow = T), axes=FALSE)
      if (count == 0){
        title(paste0("Immagine più vicina id ", vettore[id[h]]))
      } else{
        title(paste0("Immagine vicina id ", vettore[id[h]]))
      }
      
      count <- count+1
      
      somma <- somma + sqrt(rowSums((xiast - matrix(zi, ncol = 400, nrow = 1, byrow = T))^2))
      
      if (count == 6){ 
        break
      }
      
    }
    
    z <- z[- (which(distanceL2 == min(distanceL2)))]
    distanze[i+1] <- somma / count
    distanceL2 <- distanceL2[- (which(distanceL2 == min(distanceL2)))]
    
  }
  
}

view(distanze)

spark_disconnect(sc)
