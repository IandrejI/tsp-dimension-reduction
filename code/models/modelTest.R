library("tidyverse")
library("keras")
library("vip")
library("ggthemes")
library("firatheme")
library("recipes")
library("TSP")

# Load Keras Model
finalModel <- load_model_tf("models/Model2")
getwd()



# functions ---------------------------------------------------------------

savePred <- function(data,name, n){
  write_delim(as.data.frame(matrix(data$predA, nrow = n, byrow = TRUE)),paste0(name,"_predA.txt"), col_names = FALSE)
  write_delim(as.data.frame(matrix(data$predB, nrow = n, byrow = TRUE)),paste0(name,"_predB.txt"), col_names = FALSE)
  write_delim(as.data.frame(matrix(data$predC, nrow = n, byrow = TRUE)),paste0(name,"_predC.txt"), col_names = FALSE)
}

# big702 ------------------------------------------------------------------
big702_atsp <- read_delim("atspData/big702/big702.txt", delim = " ", col_names = FALSE)
big702_atsp <- as.matrix(big702_atsp[1:702])
colnames(big702_atsp) <- row.names(big702_atsp)


                        
start = Sys.time()
big702 <- atspTibbleNoSvec(big702_atsp)
end = Sys.time()
benchBig702 <- end - start
benchBig702*60
           
big702 <- predFun(big702)
remSizeBig702<- big702 %>%
summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))

savePred(big702, "big702", 702)



# rbg443 ------------------------------------------------------------------

rbg443_atsp <- t(as.matrix(read_TSPLIB("atspData/rbg443/rbg443.atsp")))

a <- Sys.time()
rbg443Test <- atspTibbleNoSvec(rbg443_atsp)
b <- Sys.time()
benchRbg443 <- (b-a)*60

rbg443pred <- predFun(rbg443Test)
remSizeRbg443 <- rbg443pred %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(rbg443pred, "rbg443", 443)




# nd163742
nd163742_atsp <- read_delim("atspData/nd163742/nd163742.txt", delim = ",", col_names = FALSE)
nd163742_atsp <- as.matrix(nd163742_atsp[,1:301])

a <- Sys.time()
nd163742 <- atspTibbleNoSvec(nd163742_atsp)
b <- Sys.time()
benchNd163742 <- (b-a)

nd163742 <- predFun(nd163742)

remSizeNd163742 <- nd163742 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(nd163742, "nd163742", n = 301)


# nd204241 ----------------------------------------------------------------
nd204241_atsp <- read_delim("atspData/nd204241/nd204241.txt", delim = ",", col_names = FALSE)
nd204241_atsp <- as.matrix(nd204241_atsp[,1:291])

a <- Sys.time()
nd204241 <- atspTibbleNoSvec(nd204241_atsp)
b <- Sys.time()
benchNd204241 <- (b-a)

nd204241 <- predFun(nd204241)
remSizeNd204241 <- nd204241 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(nd204241, "nd204241", n = 291)


# nd204441 -----------------------------------------------------------------
nd204441_atsp <- read_delim("atspData/nd204441/nd204441.txt", delim = ",", col_names = FALSE)
nd204441_atsp <- as.matrix(nd204441_atsp)
a <- Sys.time()
nd204441 <- atspTibbleNoSvec(nd204441_atsp)
b <- Sys.time()
benchNd204441 <- (b-a)

nd204441 <- predFun(nd204441)
remSizeNd204441 <- nd204441 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(nd204441, "nd204441", n = 315)



# atex5 -------------------------------------------------------------------
atex5_atsp <- as.matrix(read_delim("atspData/atex5/atex5.txt", delim = " ", col_names = FALSE)[1:72])
a <- Sys.time()
atex5 <- atspTibbleNoSvec(atex5_atsp)
b <- Sys.time()
benchAtex5 <- (b-a)
atex5 <- predFun(atex5)
remSizeAtex5 <- atex5 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(atex5, "atex5", n = 72)


# atex8 -------------------------------------------------------------------

atex8_atsp <- as.matrix(read_delim("atspData/atex8/atex8.txt", delim = " ", col_names = FALSE)[1:600])
a <- Sys.time()
atex8 <- atspTibbleNoSvec(atex8_atsp)
b <- Sys.time()
benchAtex8 <- (b-a)*60
atex8 <- predFun(atex8)
remSizeAtex8 <- atex8 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(atex8, "atex8", n = 600)


dt1000_atsp <- read_delim("atspData/dt1000/dt1000.txt", delim = " ", col_names = FALSE)
dt1000_atsp <- matrix(dt1000_atsp$X1, ncol = 1001, byrow = TRUE)

a <- Sys.time()
dt1000 <- atspTibbleNoSvec(dt1000_atsp)
b <- Sys.time()
benchDt1000_atsp <- (b-a)
benchDt1000_atsp*60


dt1000 <- predFun(dt1000)

remSizedt1000 <- dt1000 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(dt1000, "Dt1000", n = 1001)



# sol ---------------------------------------------------------------------

rbind(remSizeRbg443,remSizeAtex5, remSizeAtex8, remSizeNd204241, remSizeNd204441,
      remSizeNd163742, remSizedt1000) %>% lapply(mean)




