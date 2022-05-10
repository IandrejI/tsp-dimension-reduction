
# packages ----------------------------------------------------------------
## Install Firatheme
### remotes::install_github("vankesteren/firatheme")
# Install Keras
#keras::install_keras(version = "gpu")
library("tidyverse")
library("keras")
library("vip")
library("ggthemes")
library("firatheme")
library("recipes")
library("TSP")

# Load Keras Model
finalModel <- load_model_tf("models/Model2")



# functions ---------------------------------------------------------------

savePred <- function(data,name, n){
  write_delim(as.data.frame(matrix(data$predA, nrow = n, byrow = TRUE)),paste0(name,"_predA.txt"), col_names = FALSE)
  write_delim(as.data.frame(matrix(data$predB, nrow = n, byrow = TRUE)),paste0(name,"_predB.txt"), col_names = FALSE)
  write_delim(as.data.frame(matrix(data$predC, nrow = n, byrow = TRUE)),paste0(name,"_predC.txt"), col_names = FALSE)
}

# dc563 -------------------------------------------------------------------
dc563_atsp <- read_delim("atspData/dc563/dc563.txt", delim = ",", col_names = FALSE)
dc563_atsp <- matrix(dc563_atsp$X1, ncol = 563, nrow = 563, byrow = TRUE)
write_delim(as.data.frame(dc563_atsp), "atspData/dc563/dc563_atsp.txt", delim = " ", col_names = F)


start = Sys.time()
dc563 <- atspTibbleNoSvec(dc563_atsp)
end = Sys.time()
benchDc563 <- end - start
benchDc563*60


dc563 <- predFunFake(dc563)
remSizeDc563 <- dc563 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))

savePred(dc563, "dc563", 563)

# dc176 -------------------------------------------------------------------

dc176_atsp <- read_delim("atspData/dc176/dc176.txt", delim = ",", col_names = FALSE)
dc176_atsp <- matrix(dc176_atsp$X1, ncol = 176, nrow = 176, byrow = TRUE)
write_delim(as.data.frame(dc176_atsp), "atspData/dc176/dc176_atsp.txt", delim = " ", col_names = F)

start = Sys.time()
dc176 <- atspTibbleNoSvec(dc176_atsp, 300*nrow(dc176_atsp))
end = Sys.time()
benchDc176 <- end - start
benchDc176*60

dc176 <- predFun(dc176)
remSizeDc176 <- dc176 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))

savePred(dc176,"dc176", 176)

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

rbg443Test <- atspTibbleNoSvec(rbg443_atsp)
rbg443pred <- predFun(rbg443Test)
remSizeRbg443 <- rbg443pred %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(rbg443pred, "rbg443", 443)



# nd204441 -----------------------------------------------------------------
nd204441_atsp <- read_delim("atspData/nd204441/nd204441.txt", delim = ",", col_names = FALSE)
nd204441_atsp <- as.matrix(nd204441_atsp)
nd204441 <- atspTibbleNoSvec(nd204441_atsp)
nd204441 <- predFun(nd204441)
remSizeNd204441 <- nd204441 %>%
  summarise(remProbSizeA = sum(predA)/nrow(.), remProbSizeB = sum(predB)/nrow(.), remProbSizeC = sum(predC)/nrow(.))
savePred(nd204441, "nd204441", n = 315)

# sol ---------------------------------------------------------------------

rbind(remSizeDc563,remSizeDc849, remSizeDt1000) %>% lapply(mean)




