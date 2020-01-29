library(M4comp2018)
library(forecast)
library(TSPred)
library(Metrics)

predictions_arima <- read.csv("predictions/predictions_arima.csv", header=TRUE, sep=",")
predictions_ets <- read.csv("predictions/predictions_ets.csv", header=TRUE, sep=",")
predictions_naive <- read.csv("predictions/predictions_naive.csv", header=TRUE, sep=",")
predictions_snaive <- read.csv("predictions/predictions_snaive.csv", header=TRUE, sep=",")
predictions_nnet <- read.csv("predictions/predictions_nnet.csv", header=TRUE, sep=",")

#######

#1 arima
#2 ets
#3 naive
#4 nnet
#5 snaive

test_index <- read.csv("probabilities/test_index.csv", header=FALSE, sep=",")

probabilities <- read.csv("probabilities/probs_amano.csv", header=FALSE, sep=",")
test_labels <- read.csv("probabilities/y_test_amano.csv", header=FALSE, sep=",")

predictions_matrix_nn <- matrix(0,nrow=length(test_labels[,1]), ncol=18)
predictions_matrix_nn <- as.data.frame(predictions_matrix_nn)
smape_index_nn <- matrix(0,nrow=length(test_labels), ncol=3)
smape_index_nn <- as.data.frame(smape_index_nn)
colnames(smape_index_nn) <- c("sMAPE", "sMAPE_naive")

smape_avg_snaive <- 0

for (i in 1:length(test_labels[,1])) {
  
  index <- test_index[i,1]  
  
  series <- monthly[[index]]$x
  test <- monthly[[index]]$xx
  actual_vals <- as.numeric(test)
  
  smape_avg_snaive <- smape_avg_snaive + sMAPE(actual_vals, predictions_snaive[i,])
  
}

smape_avg_snaive <- smape_avg_snaive/length(test_labels[,1])
#0.1589321

smape_tot<- read.csv("res_etichette/smape.csv", header=TRUE, sep=",")
#media tot
#0.1598828
smape_avg_snaive_tot <- mean(smape_tot$sMape_SNaive)

for (i in 1:length(test_labels[,1])) {
  
  probs <-probabilities[i,]
  
  index <- test_index[i,1]  
  
  series <- monthly[[index]]$x
  test <- monthly[[index]]$xx
  actual_vals <- as.numeric(test)
  
  for (j in 1:18) {
    
    predictions_matrix_nn[i,j] <- probs[1]*predictions_arima[i,j] + probs[2]*predictions_ets[i,j] + probs[3]*predictions_naive[i,j] + probs[4]*predictions_nnet[i,j] + probs[5]*predictions_snaive[i,j]
    
  }
  
  #smape_srw <- sMAPE(actual_vals, predictions_snaive[i,])
  smape_pred <- sMAPE(actual_vals, predictions_matrix_nn[i,])
  
  smape_index_nn[i,1] <- smape_pred

  if(test_labels[i,1]==1){
    smape_label <- sMAPE(actual_vals, predictions_arima[i,])
    smape_index_nn[i,2] <- smape_label
  }
  if(test_labels[i,1]==2){
    smape_label <- sMAPE(actual_vals, predictions_ets[i,])
    smape_index_nn[i,2] <- smape_label
  }
  if(test_labels[i,1]==3){
    smape_label <- sMAPE(actual_vals, predictions_naive[i,])
    smape_index_nn[i,2] <- smape_label
  }
  if(test_labels[i,1]==4){
    smape_label <- sMAPE(actual_vals, predictions_nnet[i,])
    smape_index_nn[i,2] <- smape_label
  }
  if(test_labels[i,1]==5){
    smape_label <- sMAPE(actual_vals, predictions_snaive[i,])
    smape_index_nn[i,2] <- smape_label
  }
  
}


write.table(smape_index_nn,file="smape_index_mano",sep = ", ", row.names = FALSE)
write.table(predictions_matrix_nn,file="predictions_mano.csv",sep = ", ", row.names = FALSE)


print(mean(smape_index_nn[,1]))
print(mean(smape_index_nn[,1])/smape_avg_snaive)
