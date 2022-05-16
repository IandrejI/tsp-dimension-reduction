
# Packages ----------------------------------------------------------------


library(tidyverse)
library(TSP)
library(readr)


#_atsp <- t(as.matrix(read_TSPLIB("atspData/")))
#write_delim(as.data.frame(), "atspData/", delim = " ", col_names = F)


# Functions ---------------------------------------------------------------
## Load and Save ATSP as Matrix 
loadAndSaveTSP <- function(name){
 name1 <- paste0("atspData/",name,"/",name,".txt")
 name2 <- paste0("atspData/",name,"/",name,"_atsp.txt")
 atspma <- t(as.matrix(read_TSPLIB(name1)))
 write_delim(as.data.frame(atspma), name2, delim = " ", col_names = F)
 return(atspma)
}

## Erstelle Tibble mit Kantenname, Von, Zu und Cplex Lösungsvektor
atspTibble <- function(atsp, nSamples = 100*nrow(atsp)){
  
  is.matrix(atsp) || return("not a matrix")
  weight <- as.vector(t(atsp))
  n <- ncol(atsp)
  string <- deparse(substitute(atsp))
  solVec <- read.delim(paste0("atspData/",str_split(string,"_")[[1]][1],"/",str_replace(string, "atsp", "svec.txt")),
                       skip = 1, header = FALSE, col.names = "V1")
  
  name = NA
  from = NA
  to = NA
  i = 1
  
  for(a in 1:n){
    for(b in 1:n){
      name[i] <- paste(a,b, sep = " : ")
      from[i] <- a
      to[i] <- b
      i <- i+1
    }
  }
  tbl1 <- tibble(name, from, to, weight, weightZScore = NA)
  tempWeight <- tbl1$weight[tbl1$from != tbl1$to]
  tbl1$weightZScore[tbl1$from != tbl1$to] <- (tempWeight - mean(tempWeight))/sd(tempWeight)
  
  tbl1$svec <- ifelse(tbl1$name %in% solVec$V1, 1,0)
  
  # Graph Features
  tbl1$f1 <- NA
  tbl1$f2 <- NA
  tbl1$f3 <- NA
  tbl1$f4 <- NA
  for(i in unique(tbl1$from)){
    min_ik <- min(tbl1$weight[tbl1$from == i & tbl1$to != i])
    max_ik <- max(tbl1$weight[tbl1$from == i & tbl1$to != i])
    tbl1$f1[tbl1$from == i] <- (tbl1$weight[tbl1$from == i] - min_ik)/(max_ik - min_ik)
    tbl1$f3[tbl1$from == i] <- (tbl1$weight[tbl1$from == i] - sum(tbl1$weight[tbl1$from == i & tbl1$to != i])/(n-1))/(max_ik - min_ik)
    min_kj <- min(tbl1$weight[tbl1$from != i & tbl1$to == i])
    max_kj <- max(tbl1$weight[tbl1$from != i & tbl1$to == i])
    tbl1$f2[tbl1$to == i] <- (tbl1$weight[tbl1$to == i] - min_kj)/(max_kj - min_kj)
    tbl1$f4[tbl1$to == i] <- (tbl1$weight[tbl1$to == i] - sum(tbl1$weight[tbl1$from != i & tbl1$to == i])/(n-1))/(max_kj - min_kj)
  }
  
  
  #Stat Features
  
  # Create empty List and data-frame for storage
  Samples <- list()
  objective <- data.frame(value = rep(NA,nSamples), sample = rep(NA, nSamples))
  
  # For loop Zufallsroute über alle Knoten mit Start und Endpunkt 1. Setze die binäre Variable x[i][j]
  # auf 1 (bedeutet Kante X[i][j] teil der Zufallsroute). Kalkuliere in sum die Streckenlänge.
  # Speichere Route s in allSamples[s], die Stecke in objective$value[s] und die Sample Nr s in 
  # objective$sample[s]
  X <- matrix(0,nrow = n, ncol = n)
  
  for(s in 1:nSamples){
    sample <- c(1,sample(2:n),1)
    Samples[[s]] <- sample
    sum <- 0
    
    
    for(idx in 1:n){
      i <- sample[idx]
      j <- sample[idx+1]
      X[i,j] <- X[i,j] + 1
      sum = sum + atsp[i,j]
    }
    objective$value[s] <- sum
    objective$sample[s] <- s
  }
  
  
  # mean, diff, variance from objective Value
  yMean <- mean(objective$value)
  yDiff <- sum(objective$value - yMean)
  yVar <- var(objective$value)
  yQuantiles <- quantile(objective$value)
  
  # Sortiere das objective aufteigend nach dem Value
  objectiveRanked <- objective[order(objective$value),]
  
  
  # Setze Rangvariable 
  objectiveRanked$rank <- 1:nSamples
  
  # Kalkuliere die Summe der X[i][j] dividiert durch den Rang 
  fA <-  matrix(0,nrow = n, ncol = n)
  S <- fA
  XMean <- fA
  A <- fA
  B <- fA
  C <- fA
  D <- fA
  
  for(s in 1:nSamples){
    no <- objectiveRanked$sample[s]  
    sample <- Samples[[no]]
    
    for(idx in 1:n){
      i <- sample[idx]
      j <- sample[idx+1]
      XMean[i,j] <- XMean[i,j] + 1/nSamples
      fA[i,j] <- fA[i,j] + 1/s
      S[i,j] <- S[i,j] + (objective$value[no] - yMean)
      if(objective$value[no] <= yQuantiles[2]){
        A[i,j] <- A[i,j] + 1
      } else if(objective$value[no] <= yQuantiles[3]){
        B[i,j] <- B[i,j] + 1
      } else if(objective$value[no] <= yQuantiles[4]){
        C[i,j] <- C[i,j] + 1
      } else{
        D[i,j] <- D[i,j] + 1
      }
    }
  }
  
  
  covXY <- matrix(NA,nrow = n, ncol = n)
  varX <- covXY
  PC <- covXY
  fC <- covXY
  
  for(i in 1:n){
    for(j in 1:n){
      if(i != j){
        xMean <- XMean[i,j]
        sij <- S[i,j]
        covXY[i,j] <- (1 - xMean)*sij-xMean*(yDiff-sij)
        varX[i,j] <- xMean*(1 - xMean)*nSamples
        PC[i,j] <- covXY[i,j]/sqrt(varX[i,j] * yVar)
        if(A[i,j] > B[i,j] && A[i,j] > C[i,j] && A[i,j] > D[i,j]){
          fC[i,j] <- "A"
        } else if(B[i,j] > A[i,j] && B[i,j] > C[i,j] && B[i,j] > D[i,j]){
          fC[i,j] <- "B"
        } else if(C[i,j] > A[i,j] && C[i,j] > B[i,j] && C[i,j] > D[i,j]){
          fC[i,j] <- "C"
        } else if(D[i,j] > A[i,j] && D[i,j] > B[i,j] && D[i,j] > C[i,j]){
          fC[i,j] <- "D"
        } else{
          fC[i,j] <- "I"
        }
      }
    }
  }
  
  fA <- fA/max(fA, na.rm = TRUE)
  PC <- PC/min(PC, na.rm = TRUE)
 
  
  
  tbl1$f5 <- as.vector(t(fA))
  tbl1$f6 <- as.vector(t(PC))
  tbl1$f7 <- as.factor(as.vector(t(fC)))
 
  return(tbl1)
}
atspTibbleNoSvec <- function(atsp, nSamples = 100*nrow(atsp)){
  
  is.matrix(atsp) || return("not a matrix")
  weight <- as.vector(t(atsp))
  n <- ncol(atsp)
  
  name = NA
  from = NA
  to = NA
  i = 1
  
  for(a in 1:n){
    for(b in 1:n){
      name[i] <- paste(a,b, sep = " : ")
      from[i] <- a
      to[i] <- b
      i <- i+1
    }
  }
  tbl1 <- tibble(name, from, to, weight, weightZScore = NA)
  tempWeight <- tbl1$weight[tbl1$from != tbl1$to]
  tbl1$weightZScore[tbl1$from != tbl1$to] <- (tempWeight - mean(tempWeight))/sd(tempWeight)
  
  # Graph Features
  tbl1$f1 <- NA
  tbl1$f2 <- NA
  tbl1$f3 <- NA
  tbl1$f4 <- NA
  for(i in unique(tbl1$from)){
    min_ik <- min(tbl1$weight[tbl1$from == i & tbl1$to != i])
    max_ik <- max(tbl1$weight[tbl1$from == i & tbl1$to != i])
    tbl1$f1[tbl1$from == i] <- (tbl1$weight[tbl1$from == i] - min_ik)/(max_ik - min_ik)
    tbl1$f3[tbl1$from == i] <- (tbl1$weight[tbl1$from == i] - sum(tbl1$weight[tbl1$from == i & tbl1$to != i])/(n-1))/(max_ik - min_ik)
    min_kj <- min(tbl1$weight[tbl1$from != i & tbl1$to == i])
    max_kj <- max(tbl1$weight[tbl1$from != i & tbl1$to == i])
    tbl1$f2[tbl1$to == i] <- (tbl1$weight[tbl1$to == i] - min_kj)/(max_kj - min_kj)
    tbl1$f4[tbl1$to == i] <- (tbl1$weight[tbl1$to == i] - sum(tbl1$weight[tbl1$from != i & tbl1$to == i])/(n-1))/(max_kj - min_kj)
  }
  
  
  #Stat Features
  
  
  # Create empty List and data-frame for storage
  Samples <- list()
  objective <- data.frame(value = rep(NA,nSamples), sample = rep(NA, nSamples))
  
  # For loop Zufallsroute über alle Knoten mit Start und Endpunkt 1. Setze die binäre Variable x[i][j]
  # auf 1 (bedeutet Kante X[i][j] teil der Zufallsroute). Kalkuliere in sum die Streckenlänge.
  # Speichere Route s in allSamples[s], die Stecke in objective$value[s] und die Sample Nr s in 
  # objective$sample[s]
  X <- matrix(0,nrow = n, ncol = n)
  
  for(s in 1:nSamples){
    sample <- c(1,sample(2:n),1)
    Samples[[s]] <- sample
    sum <- 0
    
    
    for(idx in 1:n){
      i <- sample[idx]
      j <- sample[idx+1]
      X[i,j] <- X[i,j] + 1
      sum = sum + atsp[i,j]
    }
    objective$value[s] <- sum
    objective$sample[s] <- s
  }
  
  
  # mean, diff, variance from objective Value
  yMean <- mean(objective$value)
  yDiff <- sum(objective$value - yMean)
  yVar <- var(objective$value)
  yQuantiles <- quantile(objective$value)
  
  # Sortiere das objective aufteigend nach dem Value
  objectiveRanked <- objective[order(objective$value),]
  
  
  # Setze Rangvariable 
  objectiveRanked$rank <- 1:nSamples
  
  # Kalkuliere die Summe der X[i][j] dividiert durch den Rang 
  fA <-  matrix(0,nrow = n, ncol = n)
  S <- fA
  XMean <- fA
  A <- fA
  B <- fA
  C <- fA
  D <- fA
  
  for(s in 1:nSamples){
    no <- objectiveRanked$sample[s]  
    sample <- Samples[[no]]
    
    for(idx in 1:n){
      i <- sample[idx]
      j <- sample[idx+1]
      XMean[i,j] <- XMean[i,j] + 1/nSamples
      fA[i,j] <- fA[i,j] + 1/s
      S[i,j] <- S[i,j] + (objective$value[no] - yMean)
      if(objective$value[no] <= yQuantiles[2]){
        A[i,j] <- A[i,j] + 1
      } else if(objective$value[no] <= yQuantiles[3]){
        B[i,j] <- B[i,j] + 1
      } else if(objective$value[no] <= yQuantiles[4]){
        C[i,j] <- C[i,j] + 1
      } else{
        D[i,j] <- D[i,j] + 1
      }
    }
  }
  
  
  covXY <- matrix(NA,nrow = n, ncol = n)
  varX <- covXY
  PC <- covXY
  fC <- covXY
  
  for(i in 1:n){
    for(j in 1:n){
      if(i != j){
        xMean <- XMean[i,j]
        sij <- S[i,j]
        covXY[i,j] <- (1 - xMean)*sij-xMean*(yDiff-sij)
        varX[i,j] <- xMean*(1 - xMean)*nSamples
        PC[i,j] <- covXY[i,j]/sqrt(varX[i,j] * yVar)
        if(A[i,j] > B[i,j] && A[i,j] > C[i,j] && A[i,j] > D[i,j]){
          fC[i,j] <- "A"
        } else if(B[i,j] > A[i,j] && B[i,j] > C[i,j] && B[i,j] > D[i,j]){
          fC[i,j] <- "B"
        } else if(C[i,j] > A[i,j] && C[i,j] > B[i,j] && C[i,j] > D[i,j]){
          fC[i,j] <- "C"
        } else if(D[i,j] > A[i,j] && D[i,j] > B[i,j] && D[i,j] > C[i,j]){
          fC[i,j] <- "D"
        } else{
          fC[i,j] <- "I"
        }
      }
    }
  }
  
  fA <- fA/max(fA, na.rm = TRUE)
  PC <- PC/min(PC, na.rm = TRUE)
  
  
  tbl1$f5 <- as.vector(t(fA))
  tbl1$f6 <- as.vector(t(PC))
  tbl1$f7 <- as.factor(as.vector(t(fC)))
  
  return(tbl1)
}

# br17 --------------------------------------------------------------------

br17_atsp <- t(as.matrix(read_TSPLIB("atspData/br17/br17.atsp")))
#write_delim(as.data.frame(br17_atsp), "atspData/br17/br17_atsp.txt", delim = " ", col_names = F)

br17 <- atspTibble(br17_atsp)

# ft53 --------------------------------------------------------------------
ft53_atsp <- t(as.matrix(read_TSPLIB("atspData/ft53/ft53.atsp")))
#write_delim(as.data.frame(ft53_atsp), "atspData/ft53/ft53_atsp.txt", delim = " ", col_names = F)
ft53 <- atspTibble(ft53_atsp)


# ft70 --------------------------------------------------------------------
ft70_atsp <- t(as.matrix(read_TSPLIB("atspData/ft70/ft70.atsp")))
#write_delim(as.data.frame(ft70_atsp), "atspData/ft70/ft70_atsp.txt", delim = " ", col_names = F)
ft70 <- atspTibble(ft70_atsp)


# ftv170 ------------------------------------------------------------------
ftv170_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv170/ftv170.atsp")))
#write_delim(as.data.frame(ftv170_atsp), "atspData/ftv170/ftv170_atsp.txt", delim = " ", col_names = F)
ftv170 <- atspTibble(ftv170_atsp)

# ftv33 -------------------------------------------------------------------
ftv33_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv33/ftv33.atsp")))
#write_delim(as.data.frame(ftv33_atsp), "atspData/ftv33/ftv33_atsp.txt", delim = " ", col_names = F)
ftv33 <- atspTibble(ftv33_atsp)

# ftv35 -------------------------------------------------------------------
ftv35_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv35/ftv35.atsp")))
#write_delim(as.data.frame(ftv35_atsp), "atspData/ftv35/ftv35_atsp.txt", delim = " ", col_names = F)
ftv35 <- atspTibble(ftv35_atsp)


# ftv38 -------------------------------------------------------------------
ftv38_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv38/ftv38.atsp")))
#write_delim(as.data.frame(ftv38_atsp), "atspData/ftv38/ftv38_atsp.txt", delim = " ", col_names = F)
ftv38 <- atspTibble(ftv38_atsp)

# ftv44 -------------------------------------------------------------------
ftv44_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv44/ftv44.atsp")))
#write_delim(as.data.frame(ftv44_atsp), "atspData/ftv44/ftv44_atsp.txt", delim = " ", col_names = F)
ftv44 <- atspTibble(ftv44_atsp)

# ftv47 -------------------------------------------------------------------
ftv47_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv47/ftv47.atsp")))
#write_delim(as.data.frame(ftv47_atsp), "atspData/ftv47/ftv47_atsp.txt", delim = " ", col_names = F)

## 2 Solutions
ftv47A_atsp <- ftv47_atsp
ftv47B_atsp <- ftv47_atsp
 
ftv47A <- atspTibble(ftv47A_atsp)
ftv47B <- atspTibble(ftv47B_atsp)

# ftv55 -------------------------------------------------------------------
ftv55_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv55/ftv55.atsp")))
#write_delim(as.data.frame(ftv55_atsp), "atspData/ftv55/ftv55_atsp.txt", delim = " ", col_names = F)
ftv55 <- atspTibble(ftv55_atsp)

# ftv64 -------------------------------------------------------------------
ftv64_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv64/ftv64.atsp")))
#write_delim(as.data.frame(ftv64_atsp), "atspData/ftv64/ftv64_atsp.txt", delim = " ", col_names = F)
ftv64 <- atspTibble(ftv64_atsp)

# ftv70 -------------------------------------------------------------------
ftv70_atsp <- t(as.matrix(read_TSPLIB("atspData/ftv70/ftv70.atsp")))
#write_delim(as.data.frame(ftv70_atsp), "atspData/ftv70/ftv70_atsp.txt", delim = " ", col_names = F)
ftv70 <- atspTibble(ftv70_atsp)

# kro124p -----------------------------------------------------------------
kro124p_atsp <- t(as.matrix(read_TSPLIB("atspData/kro124p/kro124p.atsp")))
#write_delim(as.data.frame(kro124p_atsp), "atspData/kro124p/kro124p_atsp.txt", delim = " ", col_names = F)
kro124p <- atspTibble(kro124p_atsp)


# rbg323 ------------------------------------------------------------------
rbg323_atsp <- t(as.matrix(read_TSPLIB("atspData/rbg323/rbg323.atsp")))
#write_delim(as.data.frame(rbg323_atsp), "atspData/rbg323/rbg323_atsp.txt", delim = " ", col_names = F)
rbg323 <- atspTibble(rbg323_atsp)


# rbg358 ------------------------------------------------------------------
rbg358_atsp <- t(as.matrix(read_TSPLIB("atspData/rbg358/rbg358.atsp")))
#write_delim(as.data.frame(rbg358_atsp), "atspData/rbg358/rbg358_atsp.txt", delim = " ", col_names = F)
rbg358 <- atspTibble(rbg358_atsp)

# rbg403 ------------------------------------------------------------------
rbg403_atsp <- t(as.matrix(read_TSPLIB("atspData/rbg403/rbg403.atsp")))
#write_delim(as.data.frame(rbg403_atsp), "atspData/rbg403/rbg403_atsp.txt", delim = " ", col_names = F)
rbg403 <- atspTibble(rbg403_atsp)

# ry48p -------------------------------------------------------------------
ry48p_atsp <- t(as.matrix(read_TSPLIB("atspData/ry48p/ry48p.atsp")))
#write_delim(as.data.frame(ry48p_atsp), "atspData/ry48p/ry48p_atsp.txt", delim = " ", col_names = F)

ry48pv <- atspTibble(ry48p_atsp)



# nd4940 n = 67 ------------------------------------------------------------------
nd4940_atsp <- as.matrix(read_delim("atspData/nd4940/nd4940.txt", delim = ",", col_names = FALSE))
nd4940 <- atspTibble(nd4940_atsp)


# nd61641 95-----------------------------------------------------------------
nd61641_atsp <- as.matrix(read_delim("atspData/nd61641/nd61641.txt", delim = ",", col_names = FALSE))
nd61641 <- atspTibble(nd61641_atsp)

# nd41143 NEuLADEN-----------------------------------------------------------------
nd41143_atsp <- as.matrix(read_delim("atspData/nd41143/nd41143.txt", delim = ",", col_names = FALSE))
nd41143 <- atspTibble(nd41143_atsp)


# nd81741 112 -----------------------------------------------------------------
nd81741_atsp <- as.matrix(read_delim("atspData/nd81741/nd81741.txt", delim = ",", col_names = FALSE))
nd81741 <- atspTibble(nd81741_atsp)

# nd82041 -----------------------------------------------------------------
nd82041_atsp <- as.matrix(read_delim("atspData/nd82041/nd82041.txt", delim = ",", col_names = FALSE))
nd82041 <- atspTibble(nd82041_atsp)


# nd122941 ----------------------------------------------------------------
nd122941_atsp <- as.matrix(read_delim("atspData/nd122941/nd122941.txt", delim = ",", col_names = FALSE))
nd122941 <- atspTibble(nd122941_atsp)




# nd163442 ----------------------------------------------------------------
nd163442_atsp <- as.matrix(read_delim("atspData/nd163442/nd163442.txt", delim = ",", col_names = FALSE))
nd163442 <- atspTibble(nd163442_atsp)


# atex1 -------------------------------------------------------------------
atex1_atsp <- as.matrix(read_delim("atspData/atex1/atex1.txt", delim = " ", col_names = FALSE)[1:16])
atex1 <- atspTibble(atex1_atsp)

# atex3 -------------------------------------------------------------------
atex3_atsp <- as.matrix(read_delim("atspData/atex3/atex3.txt", delim = " ", col_names = FALSE)[1:32])
atex3 <- atspTibble(atex3_atsp)

# atex4 -------------------------------------------------------------------
atex4_atsp <- as.matrix(read_delim("atspData/atex4/atex4.txt", delim = " ", col_names = FALSE)[1:48])
atex4 <- atspTibble(atex4_atsp)


# Bind all Rows -----------------------------------------------------------
dfs <- sapply(.GlobalEnv, is.data.frame)
allATSP <- do.call(rbind,mget(names(dfs)[dfs]))
