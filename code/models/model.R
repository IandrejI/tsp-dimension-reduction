## install keras
##keras::install_keras(method = "gpu")

library("keras")
library("tidyverse")
library("recipes")
library("rsample")
library("caret")

# ETA ---------------------------------------------------------------------
modelData <- allATSP %>% 
  filter(!is.na(weightZScore)) %>% 
  select(-c(name, from, to, weight,weightZScore))


modelData %>% 
  summarise(nPos = sum(svec == 1), nNeg = sum(svec == 0), nPos/nNeg, P = nPos/nNeg*100)

ggplot(data = modelData, aes(x = svec)) +
  geom_bar()

recipeATSP <- recipe(svec ~ ., data = modelData) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

prepATSP <- prep(recipeATSP)

modelData <- bake(prepATSP, modelData)

corMatrix <- round(cor(modelData),4)

corrplot(corMatrix, method = "number")

# Train/Valid/Test --------------------------------------------------------

# Initialise Split mit 70% Training und 30% Test
set.seed(37978)
initSplit <-  initial_split(modelData, prop = 0.8, strata = "svec")

trainSet <- training(initSplit)
testSet <- testing(initSplit)

# Splitte Training und Validation mit 80% Training
set.seed(23346)
initSplit2 <-  initial_split(trainSet, prop = 0.8, strata = "svec")
trainSet <- training(initSplit2)
validSet <- testing(initSplit2)


# Traingsdaten
yTrain <- trainSet$svec 
xTrain <- as.matrix(trainSet %>% 
  select(-svec))
# Testdaten
yTest <- testSet$svec 
xTest <- as.matrix(testSet %>% 
                      select(-svec))
# Validationsdaten
yValid <- validSet$svec 
xValid <- as.matrix(validSet %>% 
                      select(-svec))
  

# Neuronales Netz ---------------------------------------------------------

#Drei Grid-Searches mit 2, 3 und 4-Hidden-layern. 
#

## Nicht der eigentliche Grid-Search. Eigentliche Grid-Search war umfangreicher.


hyperPara <- expand.grid(
  actiFunctions = c("relu", "tanh"),
  weights = c(226),
  loss = NA,
  auc = NA,
  recall = NA,
  valLoss = NA,
  valAuc = NA,
  valRecall = NA
)

hyperPara2 <- expand.grid(
  actiFunctions = c("relu", "tanh"),
  weights = c(226),
  loss = NA,
  auc = NA,
  recall = NA,
  valLoss = NA,
  valAuc = NA,
  valRecall = NA
)

hyperPara3 <- expand.grid(
  actiFunctions = c("relu","tanh"),
  weights = c(226),
  loss = NA,
  auc = NA,
  recall = NA,
  valLoss = NA,
  valAuc = NA,
  valRecall = NA
)
models = list()
fits = list()

models2 = list()
fits2 = list()

models3 = list()
fits3 = list()

for(i in 1:nrow(hyperPara)){
  
models[[i]] <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 16, activation = hyperPara$actiFunctions[i], input_shape = ncol(xTrain)) %>%
  layer_dense(units = 8, activation = hyperPara$actiFunctions[i]) %>% 
  layer_dense(units = 1, activation = "sigmoid") %>%
  # Backpropagation
  compile(
    loss = "BinaryCrossentropy",
    optimizer = "adam",
    metrics = c("AUC","Recall")
  )

fits[[i]] <- models[[i]] %>%
  fit(
    x = xTrain,
    y = yTrain,
    epochs = 400,
    batch_size = 2048,
    validation_data = list(xValid, yValid),
    verbose = TRUE,
    initial_bias = log(0.00443),
    callbacks = list(callback_early_stopping(patience = 50)),
    class_weights = list("0" = 1, "1" = hyperPara$weights[i])
  )
hyperPara$loss[i] <- tail(fits[[i]]$metrics$loss,1)
hyperPara$valLoss[i] <- tail(fits[[i]]$metrics$val_loss,1)
hyperPara$auc[i] <- tail(fits[[i]]$metrics$auc,1)
hyperPara$valAuc[i] <- tail(fits[[i]]$metrics$val_auc,1)
hyperPara$recall[i] <- tail(fits[[i]]$metrics$recall,1)
hyperPara$valRecall[i] <- tail(fits[[i]]$metrics$val_recall,1)
}
hyperPara
for(i in 1:nrow(hyperPara)){
  (counter <- counter + 1)
  models2[[i]] <- keras_model_sequential() %>%
    
    # Network architecture
    layer_dense(units = 32, activation = hyperPara$actiFunctions[i], input_shape = ncol(xTrain)) %>%
    layer_dense(units = 16, activation = hyperPara$actiFunctions[i]) %>% 
    layer_dense(units = 8, activation = hyperPara$actiFunctions[i]) %>% 
    layer_dense(units = 1, activation = "sigmoid") %>%
    # Backpropagation
    compile(
      loss = "BinaryCrossentropy",
      optimizer = "adam",
      metrics = c("AUC","Recall")
    )
  
  fits2[[i]] <- models2[[i]] %>%
    fit(
      x = xTrain,
      y = yTrain,
      epochs = 400,
      batch_size = 2048,
      validation_data = list(xValid, yValid),
      verbose = TRUE,
      initial_bias = log(0.00443),
      callbacks = list(callback_early_stopping(patience = 100)),
      class_weights = list("0" = 1, "1" = hyperPara$weights[i])
    )
  hyperPara2$loss[i] <- tail(fits2[[i]]$metrics$loss,1)
  hyperPara2$valLoss[i] <- tail(fits2[[i]]$metrics$val_loss,1)
  hyperPara2$auc[i] <- tail(fits2[[i]]$metrics$auc,1)
  hyperPara2$valAuc[i] <- tail(fits2[[i]]$metrics$val_auc,1)
  hyperPara2$recall[i] <- tail(fits2[[i]]$metrics$recall,1)
  hyperPara2$valRecall[i] <- tail(fits2[[i]]$metrics$val_recall,1)
}
hyperPara2
for(i in 1:nrow(hyperPara3)){
  models3[[i]] <- keras_model_sequential() %>%
    
    # Network architecture
    layer_dense(units = 32, activation = hyperPara3$actiFunctions[i], input_shape = ncol(xTrain)) %>%
    layer_dense(units = 16, activation = hyperPara3$actiFunctions[i]) %>% 
    layer_dense(units = 8, activation = hyperPara3$actiFunctions[i]) %>% 
    layer_dense(units = 4, activation = hyperPara3$actiFunctions[i]) %>% 
    layer_dense(units = 1, activation = "sigmoid") %>%
    # Backpropagation
    compile(
      loss = "BinaryCrossentropy",
      optimizer = "adam",
      metrics = c("AUC","Recall")
    )
  
  fits3[[i]] <- models3[[i]] %>%
    fit(
      x = xTrain,
      y = yTrain,
      epochs = 800,
      batch_size = 2048,
      validation_data = list(xValid, yValid),
      verbose = TRUE,
      initial_bias = log(0.00443),
      class_weights = list("0" = 1, "1" = hyperPara$weights[i])
    )
  hyperPara3$loss[i] <- tail(fits3[[i]]$metrics$loss,1)
  hyperPara3$valLoss[i] <- tail(fits3[[i]]$metrics$val_loss,1)
  hyperPara3$auc[i] <- tail(fits3[[i]]$metrics$auc,1)
  hyperPara3$valAuc[i] <- tail(fits3[[i]]$metrics$val_auc,1)
  hyperPara3$recall[i] <- tail(fits3[[i]]$metrics$recall,1)
  hyperPara3$valRecall[i] <- tail(fits3[[i]]$metrics$val_recall,1)
}
hyperPara3


# Auswahl des besten Modells
finalModel <- models3[[2]]
finalModel %>% 
  save_model_tf("Model2")

list.files("Model2")
finalFit <- fits3[[1]]
plot(finalFit)

