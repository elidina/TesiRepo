df_smape <- read.csv('res/smape.csv', sep=";")
df_mae <- read.csv('res/mae.csv', sep=";")
df_rmse <- read.csv('res/rmse.csv', sep=";")
df_mase<- read.csv('res/mase.csv', sep=";")

#label assignment: lowest value

models <- c("naive","snaive","arima","ets","nnet")
dimen <- length(df_smape$ID_Serie)

labels <- matrix(NA, nrow=dimen, ncol = 5)
colnames(labels) <- c("ID","sMAPE","RMSE","MAE","MASE")

for (i in 1:dimen) {
  
  row <- df_smape[i,2:6]
  index_smape <- which(row == min(row))

  row <- df_rmse[i,2:6]
  index_rmse <- which(row == min(row))

  row <- df_mae[i,2:6]
  index_mae <- which(row == min(row))

  row <- df_mase[i,2:6]
  index_mase <- which(row == min(row))

  labels[i,1] <- i
  labels[i,2] <- models[index_smape[1]]
  labels[i,3] <- models[index_rmse[1]]
  labels[i,4] <- models[index_mae[1]]
  labels[i,5] <- models[index_mase[1]]
  
}

write.table(labels, "labels.csv",sep=",", row.names = FALSE)
