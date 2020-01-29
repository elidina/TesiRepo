library(M4comp2018)
library(forecast)
library(TSPred)
library(Metrics)

predictions_arima <- read.csv("predictions_arima.csv", header=TRUE, sep=",")
predictions_ets <- read.csv("predictions_ets.csv", header=TRUE, sep=",")
predictions_naive <- read.csv("predictions_naive.csv", header=TRUE, sep=",")
predictions_snaive <- read.csv("predictions_snaive.csv", header=TRUE, sep=",")
predictions_nnet <- read.csv("predictions_nnet.csv", header=TRUE, sep=",")

#######

#1 arima
#2 ets
#3 naive
#4 nnet
#5 snaive

test_index <- read.csv("test_index.csv", header=FALSE, sep=",")

probabilities <- read.csv("probs_amano.csv", header=FALSE, sep=",")
test_labels <- read.csv("y_test_amano.csv", header=FALSE, sep=",")

predictions_matrix_nn <- matrix(0,nrow=length(test_labels[,1]), ncol=18)
predictions_matrix_nn <- as.data.frame(predictions_matrix_nn)
smape_index_nn <- matrix(0,nrow=length(test_labels), ncol=3)
smape_index_nn <- as.data.frame(smape_index_nn)
colnames(smape_index_nn) <- c("sMAPE", "sMAPE_naive")

for (i in 1:length(test_labels[,1])) {
  
  probs <-probabilities[i,]
  
  index <- test_index[i,1]  
  
  series <- monthly[[index]]$x
  test <- monthly[[index]]$xx
  actual_vals <- as.numeric(test)
  
  for (j in 1:18) {
    
    predictions_matrix_nn[i,j] <- probs[1]*predictions_arima[i,j] + probs[2]*predictions_ets[i,j] + probs[3]*predictions_naive[i,j] + probs[4]*predictions_nnet[i,j] + probs[5]*predictions_snaive[i,j]
    
  }
  
  smape_srw <- sMAPE(actual_vals, predictions_snaive[i,])
  smape_pred <- sMAPE(actual_vals, predictions_matrix_nn[i,])
  
  smape_index_nn[i,1] <- smape_pred
  smape_index_nn[i,2] <- smape_pred/smape_srw
  
  if(test_labels[i,1]==1){
    smape_label <- sMAPE(actual_vals, predictions_arima[i,])
    smape_index_nn[i,3] <- smape_label
  }
  if(test_labels[i,1]==2){
    smape_label <- sMAPE(actual_vals, predictions_ets[i,])
    smape_index_nn[i,3] <- smape_label
  }
  if(test_labels[i,1]==3){
    smape_label <- sMAPE(actual_vals, predictions_naive[i,])
    smape_index_nn[i,3] <- smape_label
  }
  if(test_labels[i,1]==4){
    smape_label <- sMAPE(actual_vals, predictions_nnet[i,])
    smape_index_nn[i,3] <- smape_label
  }
  if(test_labels[i,1]==5){
    smape_label <- sMAPE(actual_vals, predictions_snaive[i,])
    smape_index_nn[i,3] <- smape_label
  }
  
}


write.table(smape_index_nn,file="smape_index_mano",sep = ", ", row.names = FALSE)
write.table(predictions_matrix_nn,file="predictions_mano.csv",sep = ", ", row.names = FALSE)

somma <- 0
quanti <-0
for (k in 1:length(smape_index_nn[,2])) {
  
  if(is.infinite(smape_index_nn[k,2])==TRUE){
  }else{
    somma <- smape_index_nn[k,2] + somma
    quanti <- quanti + 1
  }
}

print(somma/quanti)
print(mean(smape_index_nn[,1]))
#print(mean(smape_index_nn[,3]))