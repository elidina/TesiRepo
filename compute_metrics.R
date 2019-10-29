library(M4comp2018)
library(forecast)
library(TSPred)
library(Metrics)

data(M4)
monthly <- Filter(function(l) l$period == "Monthly", M4)

dimen <- length(monthly)
start <- 1

dimen <- 10
start <- 1

metrics_mat <- matrix(0,ncol=6, nrow=dimen)
smape_table <- as.table(metrics_mat)
colnames(smape_table) <- c("ID_Serie","sMape_Naive","sMape_SNaive","sMape_Arima","sMape_ETS","sMape_NNetar")

rmse_table <- as.table(metrics_mat)
colnames(rmse_table) <- c("ID_Serie","rmse_Naive","rmse_SNaive","rmse_Arima","rmse_ETS","rmse_NNetar")

mae_table <- as.table(metrics_mat)
colnames(mae_table) <- c("ID_Serie","mae_Naive","mae_SNaive","mae_mae","mae_ETS","mae_NNetar")

mase_table <- as.table(metrics_mat)
colnames(mase_table) <- c("ID_Serie","mase_Naive","mase_SNaive","mase_mase","mase_ETS","mase_NNetar")

for (i in start:dimen) {
  
  series <- monthly[[i]]$x
  test <- monthly[[i]]$xx
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
  
  #metrics
  smape_rw <- sMAPE(actual_vals, pred_rw)
  smape_srw <- sMAPE(actual_vals, pred_srw)
  smape_arima <- sMAPE(actual_vals, pred_arima)
  smape_ets <- sMAPE(actual_vals, pred_ets)
  smape_nnet <- sMAPE(actual_vals, pred_nnet)
  
  smape_table[i,1] <- i
  smape_table[i,2] <- smape_rw
  smape_table[i,3] <- smape_srw
  smape_table[i,4] <- smape_arima
  smape_table[i,5] <- smape_ets
  smape_table[i,6] <- smape_nnet
  
  rmse_rw <- rmse(actual_vals, pred_rw)
  rmse_srw <- rmse(actual_vals, pred_srw)
  rmse_arima <- rmse(actual_vals, pred_arima)
  rmse_ets <- rmse(actual_vals, pred_ets)
  rmse_nnet <- rmse(actual_vals, pred_nnet)
  
  rmse_table[i,1] <- i
  rmse_table[i,2] <- rmse_rw
  rmse_table[i,3] <- rmse_srw
  rmse_table[i,4] <- rmse_arima
  rmse_table[i,5] <- rmse_ets
  rmse_table[i,6] <- rmse_nnet
  
  mae_rw <- mae(actual_vals, pred_rw)
  mae_srw <- mae(actual_vals, pred_srw)
  mae_arima <- mae(actual_vals, pred_arima)
  mae_ets <- mae(actual_vals, pred_ets)
  mae_nnet <- mae(actual_vals, pred_nnet)
  
  mae_table[i,1] <- i
  mae_table[i,2] <- mae_rw
  mae_table[i,3] <- mae_srw
  mae_table[i,4] <- mae_arima
  mae_table[i,5] <- mae_ets
  mae_table[i,6] <- mae_nnet
  
  mase_rw <- mase(actual_vals, pred_rw)
  mase_srw <- mase(actual_vals, pred_srw)
  mase_arima <- mase(actual_vals, pred_arima)
  mase_ets <- mase(actual_vals, pred_ets)
  mase_nnet <- mase(actual_vals, pred_nnet)
  
  mase_table[i,1] <- i
  mase_table[i,2] <- mase_rw
  mase_table[i,3] <- mase_srw
  mase_table[i,4] <- mase_arima
  mase_table[i,5] <- mase_ets
  mase_table[i,6] <- mase_nnet
  
  
}

write.table(smape_table,file="smape.csv",sep = ", ", row.names = FALSE)
write.table(rmse_table,file="rmse.csv",sep = ", ", row.names = FALSE)
write.table(mae_table,file="mae.csv",sep = ", ", row.names = FALSE)
write.table(mase_table,file="mase.csv",sep = ", ", row.names = FALSE)


