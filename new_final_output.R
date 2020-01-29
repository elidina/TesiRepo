predictions_arima <- read.csv("predictions_arima.csv", header=TRUE, sep=",")
predictions_ets <- read.csv("predictions_ets.csv", header=TRUE, sep=",")
predictions_naive <- read.csv("predictions_naive.csv", header=TRUE, sep=",")
predictions_snaive <- read.csv("predictions_snaive.csv", header=TRUE, sep=",")
predictions_nnet <- read.csv("predictions_nnet.csv", header=TRUE, sep=",")

test_index <- read.csv("test_index.csv", header=FALSE, sep=",")

probabilities <- read.csv("prob_203_aug.csv", header=FALSE, sep=",")
test_labels <- read.csv("y_test_imp_203_aug.csv", header=FALSE, sep=",")

predictions_matrix_nn <- matrix(0,nrow=length(test_labels[,1]), ncol=18)
predictions_matrix_nn <- as.data.frame(predictions_matrix_nn)
smape_index_nn <- matrix(0,nrow=length(test_labels[,1]), ncol=2)
smape_index_nn <- as.data.frame(smape_index_nn)
colnames(smape_index_nn) <- c("sMAPE", "sMAPE_naive")

for (i in 1:length(test_labels[,1])) {
  
  probs <-probabilities[i,]
  
  for (j in 1:18) {
    
    predictions_matrix_nn[i,j] <- probs[1]*predictions_arima[i,j] + probs[2]*predictions_ets[i,j] + probs[3]*predictions_naive[i,j] + probs[4]*predictions_nnet[i,j] + probs[5]*predictions_snaive[i,j]
    
  }
  
  smape_srw <- sMAPE(actual_vals, pred_srw)
  smape_pred <- sMAPE(actual_vals, predictions_matrix_nn[i,])
  
  smape_index_nn[i,1] <- smape_pred
  smape_index_nn[i,2] <- smape_pred/smape_srw
  
}

for (i in 1:length(test_labels[,1])) {
  

  
  smape_srw <- sMAPE(actual_vals, predictions_snaive[i,])
  smape_pred <- sMAPE(actual_vals, predictions_matrix_nn[i,])
  
  smape_index_nn[i,1] <- smape_pred
  smape_index_nn[i,2] <- smape_pred/smape_srw
  
}

write.table(smape_index_nn,file="smape_index_nn_203_aug.csv",sep = ", ", row.names = FALSE)
write.table(predictions_matrix_nn,file="predictions_nn_203_aug.csv",sep = ", ", row.names = FALSE)
