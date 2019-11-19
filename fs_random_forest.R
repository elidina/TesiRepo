library(randomForest)

df <- read.csv("seas_features.csv", sep=',')

df <- df[sample(nrow(df)),]

#split 80/20
dimen <- nrow(df)

train_dimen <- as.integer(dimen*80/100)
test_dimen <- dimen - train_dimen

train <- df[1:train_dimen,]
test <- df[(train_dimen+1):dimen,]

train_y <- train$label
test_y <- test$label

train$label <- NULL
test$label <- NULL

m <- randomForest(x=train, y= train_y, ntree = 500, importance = TRUE, mtry=6)
pred <- predict(m, test, type="class")

pred_table <- table(pred, test_y)

acc <- 0

tot <- c(0,0,0,0,0)
per_class <- c(0,0,0,0,0)

for (i in 1:test_dimen) {
  
  tot[test_y[i]] <- tot[test_y[i]] + 1
  
  if(round(pred[i]) == test_y[i]){
    
    per_class[test_y[i]] <- per_class[test_y[i]] + 1
    
    acc <- acc+1
  }
  
}

accuracy <- acc/(length(test_y))
accuracy_per_class <- per_class/tot
