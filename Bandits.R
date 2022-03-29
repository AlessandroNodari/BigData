rm(list = ls())

dir.work <- "./"
setwd(dir.work)
library(tidyverse)
source('./pullArm.R')


################################################################################
# INIZIALIZZAZIONE DELLE VARIABILI
################################################################################

sdtart <- -10L
nArmsEachGame <- 4L
numTrials <- 10000L 

ArmEpsGreedy <- "ArmEpsGr"
ArmUCB1 <- "UCB1"
ArmThompson <- "ArmThm"

if(exists(ArmUCB1)){
  rm(list = c(ArmUCB1))
}

if(exists(ArmEpsGreedy)){
  rm(list = c(ArmEpsGreedy))
}

if(exists(ArmThompson)){
  rm(list = c(ArmThompson))
}



################################################################################
# INIZIALIZZAZIONE DI OGNI MODELLO TIRANDO OGNI LEVA UNA VOLTA
################################################################################

for(j in seq_len(nArmsEachGame)){
  tmp <- pullArm(namePull = ArmEpsGreedy,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart) 
  
  tmp <- pullArm(namePull = ArmUCB1,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  tmp <- pullArm(namePull = ArmThompson,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
}



################################################################################
# INIZIALIZZAZIONE DELLE VARIABILI PER I MODELLI UCB E THOMPSON
################################################################################

m <- c(1,1,1,1)
alpha_t <- (get(ArmThompson)$sequence)$rewards + 1
beta_t <- 1 - (get(ArmThompson)$sequence)$rewards + 1
alpha <- 0.5



################################################################################
# CICLO SUI VARI MODELLI
################################################################################

for(j in seq_len(numTrials)){
  
  if(j %% 200 == 0){
    cat("Num trials ",j,"out of",numTrials,"\n")
  }
  
  ##############################################################################
  # EPSILON GREEDY
  ##############################################################################
  
  exploration <- runif(1) < 16/j
  
  if(exploration){
    tmp <- pullArm(namePull = ArmEpsGreedy,
                   nArm = sample.int(nArmsEachGame,size = 1),
                   nPlayGame = 1,
                   seed_start = sdtart) 
  } else {
    bestSoFar <- get(ArmEpsGreedy)$stats %>% 
      filter(meanArmSoFar == max(meanArmSoFar)) 
    
    tmp <- pullArm(namePull = ArmEpsGreedy,
                   nArm = bestSoFar$numArm,
                   nPlayGame = 1,
                   seed_start = sdtart)
  }
  
  ##############################################################################
  # UCB1
  ##############################################################################
  
  mu <- (get(ArmUCB1)$stats)$meanArmSoFar
  
  ucb <- mu + alpha * sqrt(2*log(j)/m)
  
  argucb <- which(ucb == max(ucb))
  
  tmp <- pullArm(namePull = ArmUCB1,
                 nArm = argucb,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  m[argucb] <- m[argucb] + 1
  
  ##############################################################################
  # THOMPSON
  ##############################################################################
  
  theta <- c(0,0,0,0)
  
  for (i in 1:nArmsEachGame){
    theta[i] <- rbeta(n = 1, shape1 = alpha_t[i], shape2 = beta_t[i])
  }
  
  arg_th <- which(theta == max(theta))
  
  tmp <- pullArm(namePull = ArmThompson,
                 nArm = arg_th,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  npl <- (get(ArmThompson)$stats)$numPlayed[arg_th]
  rsf <- (get(ArmThompson)$stats)$meanArmSoFar[arg_th]
  alpha_t[arg_th] <- rsf * npl + alpha_t[arg_th]
  beta_t[arg_th] <- npl * (1 - rsf) + beta_t[arg_th]
  
}



################################################################################
# PRINT RIASSUNTIVO DEI MODELLI
################################################################################

get(ArmEpsGreedy)
get(ArmUCB1)
get(ArmThompson)



################################################################################
# ESTRAZIONE ED ELABORAZIONI DEI DATI SULLE PERFORMANCE
################################################################################

seqEpsGreedy <- get(ArmEpsGreedy)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(rewards)/numGame) %>%
  add_column(strategy = ArmEpsGreedy)

seqUCB1 <- get(ArmUCB1)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(rewards)/numGame) %>%
  add_column(strategy = ArmUCB1)

seqThm <- get(ArmThompson)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(rewards)/numGame) %>%
  add_column(strategy = ArmThompson)

dataTimeSeries <- bind_rows(seqEpsGreedy, seqUCB1, seqThm) %>%
  mutate(numGame = numGame -nArmsEachGame) %>%
  filter(numGame > 0) 



################################################################################
# PLOT DEL GRAFICO DELLE PERFORMANCE
################################################################################

ggplot(data = dataTimeSeries, mapping = aes(x = numGame,
                                           y = meanRewards)) + 
  geom_line(aes(color = strategy, linetype = strategy)) +
  ylab("Mean of rewards")+
  xlab("Number of games")






################################################################################
# CASO DELLE INTOSSICAZIONI: UNICA DIFFERENZA USO DELLA FUNZIONE pullArmInt
################################################################################

################################################################################
# FUNZIONE LEVE PER INTOSSICAZIONE
################################################################################

pullArmInt <- function(namePull = "Pull1",nArm = 1L,seed_start = 0L, nPlayGame = 1){
  if(!exists(namePull,envir = .GlobalEnv)){
    assign(namePull, list(sequence = tibble(numArm = integer(),
                                            rewards = numeric()),
                          stats = tibble(
                            numArm = integer(),
                            meanArmSoFar = numeric(),
                            numPlayed = integer()
                          )) , envir = .GlobalEnv)
  }
  
  if(nPlayGame>0){
    Pull <- get(namePull,envir = .GlobalEnv)
    HistTN <- filter(Pull$stats,numArm == nArm)
    
    if(HistTN %>% nrow()==0 ){
      Pull$stats <- add_row(Pull$stats,numArm=nArm,meanArmSoFar=0.0,numPlayed=0L)
      HistTN <- filter(Pull$stats,numArm == nArm)
    }
    
    # mean rewards: .4, .5, .5, .6, .75, .75, .7, .6, .5, .9, (20/11)*1/2, (20/12)*1/2, ...
    
    if(nArm>10){
      a <- 1
      b <- nArm/10
    } else {
      switch (nArm,
              {a=8
              b=12},
              {a=1
              b=1},
              {a=0.05
              b=0.05},
              {a=.6
              b=.4},
              {a=.75
              b=.25},
              {a=3
              b=1},
              {a=.7
              b=.3},
              {a=1.2
              b=.8},
              {a=.5
              b=.5},
              {a=1.8
              b=.2}
      )
    }
    
    set.seed(seed = 1e7*nArm + HistTN$numPlayed + seed_start)
    rewards <- rbeta(n = nPlayGame,shape1 = a,shape2 = b)
    
    rewards <- ifelse(rewards < 0.2, 0, 1)
    
    Pull$sequence <- bind_rows(Pull$sequence,
                               tibble(numArm = nArm,
                                      rewards = rewards))
    
    sumRewSofar <- sum(rewards,HistTN$meanArmSoFar*HistTN$numPlayed)
    
    #  Pull
    
    Pull$stats <- Pull$stats %>% mutate(
      numPlayed = case_when(
        numArm == nArm ~ numPlayed + nPlayGame,
        TRUE ~ numPlayed + 0),
      meanArmSoFar = case_when(
        numArm == nArm ~ sumRewSofar/numPlayed,
        TRUE ~ meanArmSoFar)
    )
    
    assign(namePull, Pull , envir = .GlobalEnv)
    return(rewards)
  }
  
  return()
}

if(exists(ArmUCB1)){
  rm(list = c(ArmUCB1))
}

if(exists(ArmEpsGreedy)){
  rm(list = c(ArmEpsGreedy))
}

if(exists(ArmThompson)){
  rm(list = c(ArmThompson))
}

for(j in seq_len(nArmsEachGame)){
  tmp <- pullArmInt(namePull = ArmEpsGreedy,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart) 
  
  tmp <- pullArmInt(namePull = ArmUCB1,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  tmp <- pullArmInt(namePull = ArmThompson,
                 nArm = j,
                 nPlayGame = 1,
                 seed_start = sdtart)
}

m <- c(1,1,1,1)
alpha_t <- (get(ArmThompson)$sequence)$rewards + 1
beta_t <- 1 - (get(ArmThompson)$sequence)$rewards + 1
alpha <- 0.1

for(j in seq_len(numTrials)){
  
  if(j %% 200 == 0){
    cat("Num trials ",j,"out of",numTrials,"\n")
  }
  
  ##############################################################################
  # EPSILON GREEDY
  ##############################################################################
  
  exploration <- runif(1) < 16/j
  
  if(exploration){
    tmp <- pullArmInt(namePull = ArmEpsGreedy,
                   nArm = sample.int(nArmsEachGame,size = 1),
                   nPlayGame = 1,
                   seed_start = sdtart) 
  } else {
    bestSoFar <- get(ArmEpsGreedy)$stats %>% 
      filter(meanArmSoFar == max(meanArmSoFar)) 
    
    tmp <- pullArmInt(namePull = ArmEpsGreedy,
                   nArm = bestSoFar$numArm,
                   nPlayGame = 1,
                   seed_start = sdtart)
  }
  
  ##############################################################################
  # UCB1
  ##############################################################################
  
  lambda <- (get(ArmUCB1)$stats)$meanArmSoFar
  
  ucb <- lambda + alpha * sqrt(2*log(j)/m)
  
  argucb <- which(ucb == max(ucb))

  tmp <- pullArmInt(namePull = ArmUCB1,
                 nArm = argucb[1],
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  m[argucb] <- m[argucb[1]] + 1
  
  ##############################################################################
  # THOMPSON
  ##############################################################################
  
  theta <- c(0,0,0,0)
  
  for (i in 1:nArmsEachGame){
    theta[i] <- rbeta(n = 1, shape1 = alpha_t[i], shape2 = beta_t[i])
  }
  
  arg_th <- which(theta == max(theta))
  
  tmp <- pullArmInt(namePull = ArmThompson,
                 nArm = arg_th,
                 nPlayGame = 1,
                 seed_start = sdtart)
  
  npl <- (get(ArmThompson)$stats)$numPlayed[arg_th]
  rsf <- (get(ArmThompson)$stats)$meanArmSoFar[arg_th]
  alpha_t[arg_th] <- rsf * npl + alpha_t[arg_th]
  beta_t[arg_th] <- npl * (1 - rsf) + beta_t[arg_th]
  
}


get(ArmEpsGreedy)
get(ArmUCB1)
get(ArmThompson)

seqEpsGreedy <- get(ArmEpsGreedy)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(1-rewards)/numGame) %>%
  add_column(strategy = ArmEpsGreedy)

seqUCB1 <- get(ArmUCB1)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(1-rewards)/numGame) %>%
  add_column(strategy = ArmUCB1)

seqThm <- get(ArmThompson)$sequence %>%
  rowid_to_column("numGame") %>%
  mutate(meanRewards = cumsum(1-rewards)/numGame) %>%
  add_column(strategy = ArmThompson)

# 1-REWARD PER IL GRAFICO DELLE COMPLICANZE RISPETTO ALLE SOMMINISTRAZIONI
# REWARD PER IL GRAFICO DEI SENZA COMPLICANZE RISPETTO ALLE SOMMINISTRAZIONI

dataTimeSeries <- bind_rows(seqEpsGreedy, seqUCB1, seqThm) %>%
  mutate(numGame = numGame -nArmsEachGame) %>%
  filter(numGame > 0) 


ggplot(data = dataTimeSeries, mapping = aes(x = numGame,
                                            y = meanRewards)) + 
  geom_line(aes(color = strategy, linetype = strategy)) +
  ylab("Mean of intossicated")+
  xlab("Number of somministration")









