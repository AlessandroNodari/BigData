rm(list = ls())
library(tidyverse)
library(car)
Dir.Work <- "./"
setwd(Dir.Work)



################################################################################
# TRUE PER TEST FALSE PER ESECUZIONE FINALE
################################################################################

debugMode <- FALSE

if(debugMode){
  path_Data <- "dataInTiny" 
  real_counts <- "counts_tiny.txt.gz"
} else {
  path_Data <- "dataIn"
  real_counts <- "counts.txt.gz"
}



################################################################################
# ACQUISIZIONE DEI FILE
################################################################################

nFileMax <- 5000

allFiles <- dir(path = path_Data, pattern = "./*.gz",full.names = "T")
nFiles <- min(nFileMax, length(allFiles))



################################################################################
# CREAZIONE DELLE FUNZIONI HASH E DELLA MATRICE H
################################################################################

K <- 5
J <- 1e4

hash_params <- matrix(data = c(
  3,1561,
  17,277,
  38,394,
  61,13,
  78,246),byrow = T,nrow = 5,ncol = 2)

H <- matrix(data = 0L, nrow = K, ncol = J)



################################################################################
# CICLO FOR PER AGGIORNARE LA MATRICE H
################################################################################

nTotWords <- 0L

for(j1 in seq_len(nFiles)){
  
  cat("number ",j1," out of ",nFiles,": ")
  cat("processing file",allFiles[[j1]],"\n")
  
  # here we read the data (we then analyze it as a stream data)
  streamData <- read_delim(file = allFiles[[j1]],
                           col_names = F,
                           col_types = "i",delim = " ")
  
  # objected arrived in this last file
  objects_stream <- streamData$X1
  
  # counter of total objects read so far
  nTotWords <- nTotWords + length(objects_stream)
  
  # here we apply the k - hash functions 
  for (k in seq_len(K)){
    
    # we read hash parameters
    a <- hash_params[k,1]
    b <- hash_params[k,2]
    
    hashStream <- (((a*objects_stream)+b) %% 123457) %% J
    
    # idea to speed up the code: group together all the object mapped in the same place
    hashSummary <- tibble(ai = hashStream) %>% group_by(ai) %>% count()
    
    H[k,hashSummary$ai+1L] <- H[k,hashSummary$ai+1L] + hashSummary$n # add 1L since modulus in [0,...n-1]

  }
  
}



################################################################################
# ACQUISIZIONE ED ESTRAZIONE DELLE FREQUENZE VERE
################################################################################

counts_words <- read_delim(real_counts,
                           "\t", escape_double = FALSE, col_names = FALSE,
                           trim_ws = TRUE,col_types = "ii") 

# extract object names 
different_objects <- counts_words$X1

# extract frequencies in the whole stream
Fi <- counts_words$X2



################################################################################
# COMPUTAZIONE DELLE FREQUENZE OTTENUTE CON LA MATRICE H
################################################################################

# compute hatF for any object: we start with Inf  
hatFi <- rep(x = Inf,length.out = length(different_objects))

# and then we take the minimum with the hash
for (k in seq_len(K)){
  
  a <- hash_params[k,1]
  b <- hash_params[k,2]
  
  key <- (((a * different_objects) + b) %% 123457) %% J
  
  hatFi <- pmin(hatFi, H[k,key + 1L]) 
  
}



################################################################################
# PLOT FINALE
################################################################################

ggplot(data = tibble(x = Fi, y = (hatFi-Fi)/Fi)) +
  geom_point(mapping = aes(x=x,y=y), colour = "blue", size = .125) +
  scale_y_log10() +
  scale_x_log10() +
  xlab("Object frequency") +
  ylab("Relative error in estimates") +
  annotation_logticks() +
  theme_bw() 



################################################################################
# REGRESSIONE LINEARE
################################################################################

x <- Fi[which(Fi > 1e4)]
y <- hatFi[which(Fi > 1e4)]

perc <- length(x)/length(Fi)*100

clean <- y - x

x <- x[which(clean > 0)]
y <- y[which(clean > 0)]

y <- (y - x) / x
x <- log10(x)
y <- log10(y)

linear_model <- lm(y ~ x)
summary(linear_model)



################################################################################
# VERIFICA DELLE IPOTESI
################################################################################

plot(linear_model$fitted.values, linear_model$residuals, 
     main='Residui vs valori stimati', lwd=2,
     xlab='Y stimati', ylab='Residui')

abline(h=0, lwd=2)

qqPlot(linear_model$residuals,distribution = "norm",main='QQP dei residui')



################################################################################
# PLOT FINALE REGRESSIONE
################################################################################

ggplot(data = tibble(x = x, y = y)) +
  geom_point(mapping = aes(x=x,y=y), colour = "blue", size = .125) +
  xlab("Logarithm of object frequency") +
  ylab("Logarithm of relative error in estimates") +
  annotation_logticks() +
  theme_bw()+
  geom_abline(slope = linear_model$coefficients[2], intercept = linear_model$coefficients[1])








