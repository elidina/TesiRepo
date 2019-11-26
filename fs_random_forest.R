library(randomForest)

n_trees <- 1500
m_try <- 2

df <- read.csv("seas_features.csv", sep=',')
df$label <- factor(df$label,levels = c(1,2,3,4,5), labels = c("1", "2", "3","4","5"))

ind <- sample(2, nrow(df), replace = TRUE, prob=c(0.8, 0.2))

df.rf <- randomForest(label ~ ., data=df[ind == 1,], ntrees=n_trees, mtry= m_try, importance=TRUE)

df.pred <- predict(df.rf, df[ind == 2,])

r <- table(observed = df[ind==2, "label"], predicted = df.pred)

acc <- 0
for (j in 1:5) {
  acc <- acc + r[j,j]
}

accuracy <- acc/length(df[ind == 2,1])

imp <- importance(df.rf)

write.csv(imp,"feat_importance.csv")

varImpPlot(df.rf)
