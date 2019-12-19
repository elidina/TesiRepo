library(randomForest)

n_trees <- 1500
m_try <- 10

old_df <- read.csv("feats.csv", sep=',')
labels <- factor(old_df$label,levels = c(1,2,3,4,5), labels = c("1", "2", "3","4","5"))

new_df <- read.csv("new.csv", sep=',', header=FALSE)
new_df$label <- labels

set.seed(123)
ind <- sample(2, nrow(new_df), replace = TRUE, prob=c(0.8, 0.2))

new_df.rf <- randomForest(label ~ ., data=new_df[ind == 1,])

new_df.pred <- predict(new_df.rf, new_df[ind == 2,])

r <- table(observed = new_df[ind==2, "label"], predicted = new_df.pred)

acc <- 0
for (j in 1:5) {
  acc <- acc + r[j,j]
}

accuracy <- acc/length(new_df[ind == 2,1])
accuracy

#imp <- importance(new_df.rf)
#varImpPlot(new_df.rf)
