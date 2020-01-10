library(randomForest)

random_forest_training <- function(df){
  
  set.seed(123)
  rows_ind <- sample(nrow(df))
  df <-df[rows_ind,]
  
  ind <- sample(2, nrow(df), replace = TRUE, prob=c(0.8, 0.2))
  df.rf <- randomForest(label ~ ., data=df[ind == 1,], ntrees = 1500, mtry=10)
  
  df.pred <- predict(df.rf, df[ind == 2,])
  
  r <- table(observed = df[ind==2, "label"], predicted = df.pred)
  
  acc <- 0
  for (j in 1:5) {
    acc <- acc + r[j,j]
  }
  
  accuracy <- acc/length(df[ind == 2,1])
  
  errors <- c(0,0,0,0,0)
  errors[1] <- r[1,2] + r[1,3] + r[1,4] + r[1,5]
  errors[2] <- r[2,1] + r[2,3] + r[2,4] + r[2,5]
  errors[3] <- r[3,1] + r[3,2] + r[3,4] + r[3,5]
  errors[4] <- r[4,1] + r[4,3] + r[4,2] + r[4,5]
  errors[5] <- r[5,2] + r[5,3] + r[5,4] + r[5,1]
  
  probabilities <- predict(df.rf, df[ind == 2,], type='prob')
  
  observed <- df[ind==2,]$label
  
  accuracy_prob <- 0
  
  for (i in 1:length(probabilities[,1])) {
    
    row <- probabilities[i,]
    observed_label <- observed[i]
    
    index_uno <- which(row==max(row))
    index_due <- which(row[-index_uno] == max(row[-index_uno]))
    
    if(observed_label == index_uno || observed_label == index_due){
      accuracy_prob <- accuracy_prob+1
      
    }
    
  }
  
  accuracy_prob <- accuracy_prob/length(probabilities[,1])

  imp <- importance(df.rf)
  
  results <- c(accuracy, accuracy_prob, errors, imp, probabilities)
  names(results) <- c("accuracy", "accuracyprob", "errors","imp","probs")
  
  return(results)
  
}