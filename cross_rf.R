library(randomForest)
df <- read.csv("seas_features.csv", sep=',')

df$label <- factor(df$label, levels = c(1,2,3,4,5), labels=c("1","2","3","4","5"))

n_fold <- 10
unit <- (length(df[,1]))/n_fold

mat <- matrix(0, ncol=3, nrow=n_fold)
#colnames(mat) <- c("ntrees500", "ntrees1000","ntrees1500","ntrees2000","ntrees2500")
colnames(mat) <- c("mtry2", "mtry6","mtry10","mtry15","mtry20")
res <- as.data.frame(mat)

for(i in 1:n_fold){
  
  start <- (i-1)*unit + 1
  end <- start + unit - 1
  
  test <- df[start:end,]
  train <- df[-c(start:end),]
  
  #types <- c(500,1000,1050,2000,2500)
  types <- c(2,6,10,15,20)
  for (t in 1:length(types)) {
    
    rf <- randomForest(label ~ ., data= train, mtry=types[t])
    pred <- predict(rf,test)
    r <- table(observed= test$label, predicted= pred)
    
    acc <- 0
    for (j in 1:5) {
      acc <- acc + r[j,j]
    }
    res[i,t] <- acc/unit
  }
  
  #print(i)
}

write.csv(res,"mtry.csv")