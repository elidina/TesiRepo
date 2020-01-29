test_index <- read.csv("test_index.csv", header=FALSE, sep=",")

predictions_arima <- matrix(0,nrow=length(test_index[,1]), ncol=18)
predictions_ets <- matrix(0,nrow=length(test_index[,1]), ncol=18)
predictions_naive <- matrix(0,nrow=length(test_index[,1]), ncol=18)
predictions_snaive <- matrix(0,nrow=length(test_index[,1]), ncol=18)
predictions_nnet <- matrix(0,nrow=length(test_index[,1]), ncol=18)

for (i in 1:length(test_index[,1])) {
  
  index <- test_index[i,1]
  
  series <- monthly[[index]]$x
  test <- monthly[[index]]$xx
  actual_vals <- as.numeric(test)
  
  random_walk <- naive(series,h=18)
  pred_rw <- random_walk$mean
  
  s_random_walk <- snaive(series,h=18)
  pred_srw <- s_random_walk$mean
  
  ts_arima <- auto.arima(series)
  fore_arima <- forecast(ts_arima, h=18)
  pred_arima <- fore_arima$mean
  
  ts_ets <- ets(series)
  fore_ets <- forecast(ts_ets, h=18)
  pred_ets <- fore_ets$mean
  
  ts_nnet <- nnetar(series)
  fore_nnet <- forecast(ts_nnet, h=18)
  pred_nnet <- fore_nnet$mean
    
  predictions_arima[i,] <- pred_arima
  predictions_ets[i,] <- pred_ets
  predictions_naive[i,] <- pred_rw
  predictions_snaive[i,] <- pred_srw
  predictions_nnet[i,] <- pred_nnet
    
  
  # for (j in 1:18) {
  #   
  #   predictions_arima[i,j] <- pred_arima[j]
  #   predictions_ets[i,j] <- pred_ets[j]
  #   predictions_naive[i,j] <- pred_rw[j]
  #   predictions_snaive[i,j] <- pred_srw[j]
  #   predictions_nnet[i,j] <- pred_nnet[j]
  #   
  # }
  
}

write.table(predictions_arima,file="predictions_arima.csv",sep = ", ", row.names = FALSE)
write.table(predictions_ets,file="predictions_ets.csv",sep = ", ", row.names = FALSE)
write.table(predictions_naive,file="predictions_naive.csv",sep = ", ", row.names = FALSE)
write.table(predictions_snaive,file="predictions_snaive.csv",sep = ", ", row.names = FALSE)
write.table(predictions_nnet,file="predictions_nnet.csv",sep = ", ", row.names = FALSE)
