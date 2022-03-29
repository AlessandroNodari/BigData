rm(list = ls())
library(sparklyr)
library(tidyverse)
Dir.Work <- "C:/Users/super/Downloads"
setwd(Dir.Work)

sc <- spark_connect(master = "local")

################################################################################
#CARICAMENTO DEL DATASET DELLE IMMAGINI
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
#CARICAMENTO DEL DATASET DELLE IMMAGINI TEST
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
#CALCOLO DELLE SEI IMMAGINI PIU' VICINE E DELLE DISTANZE MEDIE
################################################################################

distanze <- rep(0,20)

for (i in (1:20)){
  
  zi <- ImagesTestR %>%
        slice(i) %>%
        as.matrix()
  
  NNeigh <- ml_approx_nearest_neighbors(model = model_LSHfitted, 
                                        dataset = datasetImages_Assembled, 
                                        key = zi %>% as.vector(), 
                                        num_nearest_neighbors = 7,
                                        dist_col = "distCol")
  
  Closest <- NNeigh %>% collect()
  Closest <- Closest[-1,] #ESCLUDO L'IMMAGINE STESSA
  
  xij <- DatasetImagesR %>%
          slice(Closest$id+1) %>%
          as.matrix()
  
  distanceL2 <- sqrt(rowSums((xij - matrix(zi, ncol = 400, nrow = 6, byrow = T))^2))
  
  ##############################################################################
  #STAMPA DELLE IMMAGINI
  ##############################################################################
  
  for (j in 1:6){
    
    xiast <- xij[j,]
    
    par(mfrow=c(1,2))
    
    image(matrix(zi,ncol = 20,byrow = T), axes=FALSE)
    title(paste0("Immagine test ",i))
    
    image(matrix(xiast,ncol = 20,byrow = T), axes=FALSE)
    if (j == 1){
      title(paste0("Immagine più vicina id ",Closest$id[j]+1))
    } else{
      title(paste0("Immagine vicina id ",Closest$id[j]+1))
    }
  
  }
  
  distanze[i] <- sum(distanceL2) / 6
  
}

view(distanze)

spark_disconnect(sc)
