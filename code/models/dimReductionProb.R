savePredProb <- function(data,name, n){
  matrix <- matrix(data$pred, nrow = n, byrow = TRUE)
  matrix[is.nan(matrix)] <- 0
  write_csv(as.data.frame(matrix),paste0("atspData/",name,"/",name,"_predProb.txt"), col_names = FALSE, )
}

savePredProb(rbg443pred, "rbg443",443)
savePredProb(atex5, "atex5", 72)
savePredProb(td1000. "td1000", 1001)