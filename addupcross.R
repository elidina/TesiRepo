mtry1 <- read.csv("mtry1.csv", header=TRUE)
mtry2 <- read.csv("mtry2.csv", header=TRUE)

#MTRY

cross_accuracy_mtry <- mtry1
cross_accuracy_mtry$mtry15 <- mtry2$mtry15
cross_accuracy_mtry$mtry20 <- mtry2$mtry20
cross_accuracy_mtry$max <- "NULL"

write.csv(cross_accuracy_mtry,"mtry.csv")

for (i in 1:10) {
  
  line <- cross_accuracy_mtry[i,2:6]
  ind <- which(line==max(line))
  res <- toString(ind[1])
  if (length(ind) > 1){
    res <- paste(res, ind[2], sep="/")
  }
  cross_accuracy_mtry[i,]$max <- res
}

res_mtry <- c(0,0,0,0,0)
for (i in 1:10) {
  
  if(grepl("1",cross_accuracy_mtry$max[i])){
    res_mtry[1] <- res_mtry[1]+1
  }
  if(grepl("2",cross_accuracy_mtry$max[i])){
    res_mtry[2] <- res_mtry[2]+1
  }
  if(grepl("3",cross_accuracy_mtry$max[i])){
    res_mtry[3] <- res_mtry[3]+1
  }
  if(grepl("4",cross_accuracy_mtry$max[i])){
    res_mtry[4] <- res_mtry[4]+1
  }
  if(grepl("5",cross_accuracy_mtry$max[i])){
    res_mtry[5] <- res_mtry[5]+1
  }
  
}

########
#NTREES

ntrees1 <- read.csv("ntrees1.csv", header=TRUE)
ntrees2 <- read.csv("ntrees2.csv", header=TRUE)

cross_accuracy_ntrees <- ntrees1
cross_accuracy_ntrees$ntrees2000<- NULL
cross_accuracy_ntrees$ntrees1050 <- ntrees2$ntrees1050
cross_accuracy_ntrees$ntrees2000 <- ntrees1$ntrees2000
cross_accuracy_ntrees$ntrees2050 <- ntrees2$ntrees2050
cross_accuracy_ntrees$max <- "NULL"

write.csv(cross_accuracy_ntrees,"ntrees.csv")

for (i in 1:10) {
  
  line <- cross_accuracy_ntrees[i,2:6]
  ind <- which(line==max(line))
  res <- toString(ind[1])
  if (length(ind) > 1){
    res <- paste(res, ind[2], sep="/")
  }
  cross_accuracy_ntrees[i,]$max <- res
}

res_ntrees <- c(0,0,0,0,0)
for (i in 1:10) {
  
  if(grepl("1",cross_accuracy_ntrees$max[i])){
    res_ntrees[1] <- res_ntrees[1]+1
  }
  if(grepl("2",cross_accuracy_ntrees$max[i])){
    res_ntrees[2] <- res_ntrees[2]+1
  }
  if(grepl("3",cross_accuracy_ntrees$max[i])){
    res_ntrees[3] <- res_ntrees[3]+1
  }
  if(grepl("4",cross_accuracy_ntrees$max[i])){
    res_ntrees[4] <- res_ntrees[4]+1
  }
  if(grepl("5",cross_accuracy_ntrees$max[i])){
    res_ntrees[5] <- res_ntrees[5]+1
  }
  
}