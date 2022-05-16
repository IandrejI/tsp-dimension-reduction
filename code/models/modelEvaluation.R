library("tidyverse")
library("keras")
library("vip")
library("ggthemes")
library("firatheme")


# Berechnung der ROC auf den Test-Daten -----------------------------------

# Load Keras Model
finalModel <- load_model_tf("models/Model2")
summary(finalModel)



yPredict <- as.vector(predict(finalModel, xTest))

# ROC
rocFinalModel <- pROC::roc(yTest, as.vector(yPredict), plot = TRUE)

rocFinalModelTbl <- tibble(tpr = round(rocFinalModel$sensitivities,3),
                           tnr = round(rocFinalModel$specificities,3),
                           fpr = round(1-tnr,3),
                           gMean = round(sqrt(tpr*tnr),3),
                           threshold = round(rocFinalModel$thresholds,4)) %>% 
  arrange(desc(gMean)) %>%
  distinct(tpr, tnr, fpr, gMean,threshold) 

# Bestimmte threshold Values
thresholdValues <- rocFinalModelTbl %>% 
  filter(threshold == 0.004 |  threshold == 0.001 |  threshold == 0.0004) %>% 
  group_by(threshold = as.factor(threshold)) %>% 
  summarise(tpr = mean(tpr),fpr = mean(fpr))
thresholdValues$label <- c("A","B", "C")

# Plot ROC
ggplot(data = rocFinalModelTbl,aes(x = fpr, y = tpr)) +
  geom_line() +
  geom_point(data = thresholdValues, aes(x = fpr, y = tpr, color = threshold), size = 2) +
  geom_label(data = thresholdValues, aes(x = fpr+0.025, y = tpr-0.045), label = thresholdValues$label) +
  coord_fixed(ratio = 1) +
  scale_color_brewer(palette = "Set2", labels = c("A: 0.0004", "B: 0.001", "C: 0.004")) +
  ggtitle("ROC-Kurve", "AUC = 0.9484") +
  xlab("False positive rate") +
  ylab("True positive rate") +
  theme_fira()
ggsave("code/visuals/ROC.eps")
dev.off()

rocFinalModelTbl %>% 
  arrange(desc(gMean), desc(sens))

pred_wrapper <- function(object, newdata) {
  predict(object, newdata)
}

# Feature Importance mittels VIP ------------------------------------------

set.seed(323844)
pfiFinalModel <- vi_permute(finalModel,
                     pred_wrapper = function(object, newdata) predict(object, newdata),
                     target = yTrain,
                     train = xTrain,
                     metric = "auc",
                     reference_class = "1",
                     progress = "text",
                     verbose = TRUE
                     )

vip(pfiFinalModel,
    num_features = ncol(xTrain)) +
  theme_fira() +
  ylab("Importance") +
  xlab("Feauture") +
  ggtitle("Permutation based feature importance")
ggsave("code/visuals/PFI.eps",
       width = 5, height = 5)



# Prediction Function -----------------------------------------------------

predFun <- function(data){
  tempData <- data %>% 
    select(-c(name, from, to, weight))
  X <- as.matrix(bake(prepATSP, tempData))
  
  yPredict <- as.vector(predict(finalModel, X))
  
  data$pred <- yPredict
  data$predA <- ifelse(yPredict >= 0.0004, 1, 0)
  data$predA[is.na(data$predA)] <- 0
  data$predB <- ifelse(yPredict >= 0.001, 1, 0)
  data$predB[is.na(data$predB)] <- 0
  data$predC <- ifelse(yPredict >= 0.004, 1, 0)
  data$predC[is.na(data$predC)] <- 0

  
  return(data)
}

